(ns bord.data
  (:require
    [clojure.walk :refer [keywordize-keys stringify-keys postwalk]]))

(defonce loaded-db (atom nil))
(def db-name "bord-default")
(def db-version 11)
(def meta-store-name "table-meta")
(def fragment-store-name "table-fragment")
(def function-store-name "function")

; If the key is a UUID we'd rather treat it as a string
(def nested-id-maps [:columns :operations])

(defn nested-id-map->clj [entry]
  (let [id-map-keys (filter #(contains? entry %) nested-id-maps)]
    (reduce
      (fn [e k] (update-in e [k] update-vals #(update-keys % keyword)))
      entry
      id-map-keys)))

(defn stored-entry->clj [entry]
  (-> entry
      js->clj
      (update-keys #(clojure.string/replace % #"_" "-"))
      (update-keys keyword)
      nested-id-map->clj))

(defn stored->clj [data]
  (->> data
      js->clj
      (map stored-entry->clj)
      (map #(vector (:id %) %))
      (into {})))

(defn clj->stored-entry [entry]
  (-> entry
      (update-keys name)
      (update-keys #(clojure.string/replace % #"-" "_"))
      clj->js))

(defn indexed-db []
  (if (-> js/self .-document undefined?)
    js/self.indexedDB
    js/window.indexedDB))

(defn idb-key-range []
  (if (-> js/self .-document undefined?)
    js/self.IDBKeyRange
    js/window.IDBKeyRange))

(defn init-table-meta-store [db]
  (doto (.createObjectStore db meta-store-name #js {"keyPath" "id"})
    (.createIndex "name" "name" #js {"unique" false})
    (.createIndex "created" "created_at" #js {"unique" false})
    (.createIndex "updated" "updated_at" #js {"unique" false})))

(defn init-table-fragment-store [db]
  (doto (.createObjectStore db fragment-store-name #js {"keyPath" "id"})
    (.createIndex "table" "table_id" #js {"unique" false})
    (.createIndex "fragment_offset" ["table_id" "offset"] #js {"unique" true})
    (.createIndex "row" ["table_id" "first_row"] #js {"unique" true})))

(defn init-function-store [db]
  (.createObjectStore db function-store-name #js {"keyPath" "id"}))

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
    (init-function-store db)
    (reset! loaded-db db)))

(defn db-init [{:keys [on-success on-error]}]
  (let [db-request (.open (indexed-db) db-name db-version)]
    (set!
      (.-onsuccess db-request)
      #(db-init-onsuccess db-request on-success))
    (set!
      (.-onupgradeneeded db-request)
      #(db-init-onupgradeneeded % db-request on-success on-error))
    (set!
      (.-onerror db-request)
      on-error)))

(defn get-transaction [{:keys [store-names command on-complete on-error]}]
  (let [transaction (.transaction @loaded-db store-names command)]
    (set! (.-oncomplete transaction) on-complete)
    (set! (.-onerror transaction) on-error)
    transaction))

(defn read-first-cursor [store on-complete]
  (set!
    (.-onsuccess store)
    (fn [event]
      (if-let [cursor (.. event -target -result)]
        (on-complete (.-value cursor))
        (on-complete nil)))))

(defn read-all-cursor [store on-complete]
  (let [result (atom [])]
    (set!
      (.-onsuccess store)
      (fn [event]
        (if-let [cursor (.. event -target -result)]
          (do
            (swap! result conj (.-value cursor))
            (.continue cursor))
          (on-complete @result))))))

(defn delete-all-cursor [store on-complete]
  (set!
    (.-onsuccess store)
    (fn [event]
      (if-let [cursor (.. event -target -result)]
        (do
          (.delete cursor)
          (.continue cursor))
        (on-complete)))))

(defn iterative-cursor [store callback]
  (set!
    (.-onsuccess store)
    (fn [event]
      (if-let [cursor (.. event -target -result)]
        (when (callback (stored-entry->clj (.-value cursor)))
          (.continue cursor))
        (callback nil)))))

(defn read-table-fragments
  [{:as args :keys [table-id offset limit cursor-callback on-complete on-error]}]
  (let [params (clj->js [[table-id offset] [table-id (+ offset limit)]])
        cursor-range (.bound (idb-key-range)
                             (clj->js [table-id offset])
                             (clj->js [table-id (+ offset limit)])
                             false
                             true)
        transaction-params
        (-> args
            (select-keys [:on-complete :on-error])
            (assoc :store-names [fragment-store-name] :command "readonly"))]
    (-> (get-transaction transaction-params)
        (.objectStore fragment-store-name)
        (.index "fragment_offset")
        (.openCursor cursor-range)
        (iterative-cursor cursor-callback))))

(defn put-item [{:as args :keys [data store-name on-complete on-error]}]
  (let [transaction-params
        (-> args
            (select-keys [:on-complete :on-error])
            (assoc :store-names [store-name] :command "readwrite"))]
    (-> (get-transaction transaction-params)
        (.objectStore store-name)
        (.put (clj->stored-entry data)))))

(defn put-meta [args]
  (put-item (assoc args :store-name meta-store-name)))

(defn put-fragment [args]
  (put-item (assoc args :store-name fragment-store-name)))

(defn put-function [args]
  (put-item (assoc args :store-name function-store-name)))

(defn read-all-items [store-name on-complete]
  (let [transaction-params
        {:store-names [store-name]
         :command "readonly"
         :on-complete #(js/console.info "Cursor completed")
         :on-error #(js/console.error "Cursor failed: " %)}]
    (-> (get-transaction transaction-params)
        (.objectStore store-name)
        .openCursor
        (read-all-cursor #(on-complete (stored->clj %))))))

(defn read-all-tables [on-complete]
  (read-all-items meta-store-name on-complete))

(defn read-all-functions [on-complete]
  (read-all-items function-store-name on-complete))

(defn fetch-fragment [{:keys [table-id row-number on-complete]}]
  (let [params (clj->js [table-id row-number])
        cursor-range (.lowerBound (idb-key-range) params)
        transaction-params
        {:store-names [fragment-store-name]
         :command "readonly"
         :on-complete #(js/console.info "Fragment fetched")
         :on-error #(js/console.error "Fragment fetch failed: " %)}]
    (-> (get-transaction transaction-params)
        (.objectStore fragment-store-name)
        (.index "row")
        (.openCursor cursor-range)
        (read-first-cursor
          #(on-complete (some-> % stored-entry->clj))))))

(defn count-fragments [{:keys [table-id on-complete]}]
  (let [transaction-params
        {:store-names [fragment-store-name]
         :command "readonly"
         :on-complete #(js/console.info "Fragments counted")
         :on-error #(js/console.error "Fragment count failed: " %)}
        request 
        (-> (get-transaction transaction-params)
            (.objectStore fragment-store-name)
            (.index "table")
            (.count table-id))]
    (set! (.-onsuccess request) #(on-complete (.-result request)))))

(defn fetch-item [{:keys [store-name item-key on-complete on-error]}]
  (let [transaction-params
        {:store-names [store-name]
         :command "readonly"
         :on-complete #(js/console.info "Fetched: " store-name item-key)
         :on-error #(js/console.error "Fetch failed: " store-name item-key %)}
        request
        (-> (get-transaction transaction-params)
            (.objectStore store-name)
            (.get item-key))]
    (set!
      (.-onsuccess request)
      #(on-complete (some-> request .-result stored-entry->clj)))
    (set! (.-onerror request) on-error)))

(defn fetch-meta [{:as args :keys [table-id]}]
  (fetch-item
    (assoc args :item-key table-id :store-name meta-store-name)))

(defn fetch-function [{:as args :keys [function-id]}]
  (fetch-item
    (assoc args :item-key function-id :store-name function-store-name)))

(defn delete-table [{:keys [table-id on-complete]}]
  (let [store-names [meta-store-name fragment-store-name]
        transaction-params
        {:store-names store-names
         :command "readwrite"
         :on-complete on-complete
         :on-error #(js/console.error "Data deletion failed: " %)}]
    (doto (get-transaction transaction-params)
      (-> (.objectStore meta-store-name)
          (.delete table-id))
      (-> (.objectStore fragment-store-name)
          (.index "table")
          (.openCursor (.only (idb-key-range) table-id))
          (delete-all-cursor #(js/console.log "fragments deleted"))))))

(defn delete-fragments [{:keys [table-id on-complete]}]
  (let [store-names [fragment-store-name]
        transaction-params
        {:store-names store-names
         :command "readwrite"
         :on-complete on-complete
         :on-error #(js/console.error "Data deletion failed: " %)}]
    (doto (get-transaction transaction-params)
      (-> (.objectStore fragment-store-name)
          (.index "table")
          (.openCursor (.only (idb-key-range) table-id))
          (delete-all-cursor #(js/console.log "fragments deleted"))))))

(defn delete-function [{:keys [function-id on-complete]}]
  (let [transaction-params
        {:store-names [function-store-name]
         :command "readwrite"
         :on-complete on-complete
         :on-error #(js/console.error "Data deletion failed: " %)}]
    (-> (get-transaction transaction-params)
        (.objectStore function-store-name)
        (.delete function-id))))
