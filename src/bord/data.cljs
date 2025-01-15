(ns bord.data
  (:require 
    [clojure.walk :refer [keywordize-keys stringify-keys postwalk]]
    [reagent.core :as r]))

(defonce loaded-db (r/atom nil))
(def db-name "bord-default")
(def db-version 10)
(def meta-store-name "table-meta")
(def fragment-store-name "table-fragment")

(defn stored-entry->clj [entry]
  (-> entry
      js->clj
      (update-keys #(clojure.string/replace % #"_" "-"))
      (update-keys keyword)
      (update-in [:columns] update-vals #(update-keys % keyword))))

(defn stored->clj [data]
  (->> data
      js->clj
      (map stored-entry->clj)))

(defn clj->stored-entry [entry]
  (-> entry
      (update-keys name)
      (update-keys #(clojure.string/replace % #"-" "_"))
      clj->js))


(defn init-table-meta-store [db]
  (let [store (.createObjectStore
                db meta-store-name #js {"keyPath" "id"})]
    (.createIndex store "name" "name" #js {"unique" false})
    (.createIndex store "created" "created_at" #js {"unique" false})
    (.createIndex store "updated" "updated_at" #js {"unique" false})))

(defn init-table-fragment-store [db]
  (let [store (.createObjectStore
                db fragment-store-name #js {"keyPath" "id"})]
    (.createIndex store "table" "table_id" #js {"unique" false})
    (.createIndex store "row" ["table_id" "first_row"] #js {"unique" true})))

(defn db-init-onsuccess [request callback]
  (reset! loaded-db (.-result request))
  (callback))

(defn db-init-onupgradeneeded [event db-request on-success on-error]
  (let [transaction (.-transaction db-request)
        db (.. event -target -result)]
    (set! (.-oncomplete transaction) on-success)
    (set! (.-onerror db) on-error)
    (init-table-meta-store db)
    (init-table-fragment-store db)
    (reset! loaded-db db)))

(defn db-init [{:keys [on-success on-error]}]
  (let [db-request (js/window.indexedDB.open db-name db-version)]
    (set! (.-onsuccess db-request)
          #(db-init-onsuccess db-request on-success))
    (set! (.-onupgradeneeded db-request)
          #(db-init-onupgradeneeded % db-request on-success on-error))
    (set! (.-onerror db-request) on-error)))

(defn get-transaction [{:keys [store-names command on-complete on-error]}]
  (let [transaction (.transaction @loaded-db store-names command)]
    (set! (.-oncomplete transaction) on-complete)
    (set! (.-onerror transaction) on-error)
    transaction))

(defn put-meta [{:as args :keys [data on-complete on-error]}]
  (let [transaction (-> args
                        (select-keys [:on-complete :on-error])
                        (assoc :store-names [meta-store-name] :command "readwrite")
                        get-transaction)
        meta-store (.objectStore transaction meta-store-name)]
    (.put meta-store (clj->stored-entry data))))

(defn read-first-cursor [store on-complete]
  (set! (.-onsuccess store)
        (fn [event]
          (let [cursor (.. event -target -result)]
            (if (some? cursor)
              (on-complete (.-value cursor))
              (on-complete nil))))))

(defn read-all-cursor [store on-complete]
  (let [result (r/atom [])]
    (set! (.-onsuccess store)
          (fn [event]
            (let [cursor (.. event -target -result)]
              (if (some? cursor)
                (do 
                  (swap! result conj (.-value cursor))
                  (.continue cursor))
                (on-complete @result)))))))

(defn delete-all-cursor [store on-complete]
  (set! (.-onsuccess store)
        (fn [event]
          (let [cursor (.. event -target -result)]
            (if (some? cursor)
              (do
                (.delete cursor)
                (.continue cursor))
              (on-complete))))))

(defn read-table-fragments [{:as args :keys [table-id cursor-callback on-complete on-error]}]
  (let [transaction (-> args
                        (select-keys [:on-complete :on-error])
                        (assoc :store-names [fragment-store-name] :command "read")
                        get-transaction)
        store (.objectStore transaction fragment-store-name)
        index (.index store "table_id")
        cursor (.openCursor index)]
    (set! (.-onsuccess cursor) #(cursor-callback (.. % -target -result)))))

(defn put-fragment [{:as args :keys [data on-complete on-error]}]
  (let [transaction (-> args
                        (select-keys [:on-complete :on-error])
                        (assoc :store-names [fragment-store-name] :command "readwrite")
                        get-transaction)
        fragment-store (.objectStore transaction fragment-store-name)]
    (.put fragment-store (clj->stored-entry data))))

(defn compose-results [results callback]
  (if (every? #(contains? results %) '(:meta :fragments))
    (let [fragments (group-by :table-id (:fragments results))]
      (callback (map #(assoc % :rows (get-in fragments [(:id %) 0 :data])) (:meta results))))))

(defn read-all [on-complete]
  (let [results (r/atom {})
        store-names [meta-store-name fragment-store-name]
        transaction (get-transaction 
                      {:store-names store-names
                        :command "readonly"
                        :on-complete #(js/console.info "Cursor completed")
                        :on-error #(js/console.error "Cursor failed: " %)})
        on-store-complete (fn [store-key store-result]
                            (swap! results assoc
                                   store-key (stored->clj store-result))
                            (compose-results @results on-complete))]
    (read-all-cursor (.openCursor (.objectStore transaction meta-store-name))
                    #(on-store-complete :meta %))
    (read-all-cursor (.openCursor (.objectStore transaction fragment-store-name))
                    #(on-store-complete :fragments %))))

(defn read-all-tables [on-complete]
  (let [transaction-params
        {:store-names [meta-store-name]
         :command "readonly"
         :on-complete #(js/console.info "Cursor completed")
         :on-error #(js/console.error "Cursor failed: " %)}]
    (-> (get-transaction transaction-params)
        (.objectStore meta-store-name)
        .openCursor
        (read-all-cursor #(on-complete (stored->clj %))))))

(defn fetch-fragment [{:keys [table-id row-number on-complete]}]
  (let [params (clj->js [table-id, row-number])
        cursor-range (js/window.IDBKeyRange.lowerBound params)
        transaction (get-transaction 
                      {:store-names [fragment-store-name]
                        :command "readonly"
                        :on-complete #(js/console.info "Fragment fetched")
                        :on-error #(js/console.error "Fragment fetch failed: " %)})
        cursor-callback (fn [result]
                          (if (some? result)
                            (on-complete (stored-entry->clj result))
                            (on-complete nil)))]
    (-> transaction
        (.objectStore fragment-store-name)
        (.index "row")
        (.openCursor cursor-range)
        (read-first-cursor cursor-callback))))

(defn delete-table [{:keys [table-id on-complete]}]
  (let [store-names [meta-store-name fragment-store-name]
        transaction-params {:store-names store-names
                            :command "readwrite"
                            :on-complete on-complete
                            :on-error #(js/console.error "Data deletion failed: " %)}]
    (doto (get-transaction transaction-params)
      (-> (.objectStore meta-store-name)
          (.delete table-id))
      (-> (.objectStore fragment-store-name)
          (.index "table")
          (.openCursor (js/window.IDBKeyRange.only table-id))
          (delete-all-cursor #(js/console.log "fragments deleted"))))))
