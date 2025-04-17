(ns bord.data
  (:require
    [clojure.walk :refer [keywordize-keys stringify-keys postwalk]]))

(defonce loaded-db (atom nil))
(def db-name "bord-default")
(def db-version 11)
(def meta-store-name "table-meta")
(def fragment-store-name "data-fragment")
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

(defn- indexed-db []
  (if (-> js/self .-document undefined?)
    js/self.indexedDB
    js/window.indexedDB))

(defn- idb-key-range []
  (if (-> js/self .-document undefined?)
    js/self.IDBKeyRange
    js/window.IDBKeyRange))

(defn- init-table-meta-store [db]
  (doto (.createObjectStore db meta-store-name #js {"keyPath" "id"})
    (.createIndex "name" "name" #js {"unique" false})
    (.createIndex "created" "created_at" #js {"unique" false})
    (.createIndex "updated" "updated_at" #js {"unique" false})))

(defn- init-data-fragment-store [db]
  (doto (.createObjectStore db fragment-store-name #js {"keyPath" "id"})
    (.createIndex "object" "object_id" #js {"unique" false})
    (.createIndex "fragment_offset" ["object_id" "offset"] #js {"unique" true})
    (.createIndex "row_bound" ["object_id" "last_row"] #js {"unique" true})))

(defn- init-function-store [db]
  (.createObjectStore db function-store-name #js {"keyPath" "id"}))

(defn- db-init-onsuccess [request callback]
  (reset! loaded-db (.-result request))
  (callback))

(defn- db-init-onupgradeneeded [event db-request on-success on-error]
  (let [transaction (.-transaction db-request)
        db (.. event -target -result)]
    (set! (.-oncomplete transaction) on-success)
    (set! (.-onerror db) on-error)
    (init-table-meta-store db)
    (init-data-fragment-store db)
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

(defn- set-error-action [store callback]
  (set! (.-onerror store) callback)
  store)

(defn- set-success-action [store callback]
  (set! (.-onsuccess store) callback)
  store)

(defn- read-all-cursor [event result on-complete]
  (if-let [cursor (.. event -target -result)]
    (do
      (swap! result conj (.-value cursor))
      (.continue cursor))
    (on-complete (stored->clj @result))))

(defn- delete-all-cursor [event on-complete]
  (if-let [cursor (.. event -target -result)]
    (do
      (.delete cursor)
      (.continue cursor))
    (on-complete)))

(defn- iterative-cursor [event callback]
  (if-let [cursor (.. event -target -result)]
    (when (callback (stored-entry->clj (.-value cursor)))
      (.continue cursor))
    (callback nil)))

(defn- get-transaction [{:keys [store-names command on-complete on-error]}]
  (let [transaction (.transaction @loaded-db store-names command)]
    (set! (.-oncomplete transaction) on-complete)
    (set! (.-onerror transaction) on-error)
    transaction))

(defn- fetch-fragments [index cursor-range on-success]
  (let [transaction-params
        {:store-names [fragment-store-name]
         :command "readonly"
         :on-complete #(js/console.info "Fragment fetched")
         :on-error #(js/console.error "Fragment fetch failed: " %)}]
    (-> (get-transaction transaction-params)
        (.objectStore fragment-store-name)
        (.index index)
        (.openCursor cursor-range)
        (set-success-action on-success))))

(defn read-row-fragment [{:keys [object-id start-row on-success]}]
  (fetch-fragments
    "row_bound"
    (.bound (idb-key-range) (clj->js [object-id start-row]) (clj->js [object-id js/Infinity]))
    #(iterative-cursor % on-success)))

(defn read-fragments [{:keys [object-id offset limit cursor-callback]}]
  (let [params (clj->js [[object-id offset] [object-id (+ offset limit)]])
        cursor-range (.bound (idb-key-range)
                             (clj->js [object-id offset])
                             (clj->js [object-id (+ offset limit)])
                             false
                             true)]
    (fetch-fragments
      "fragment_offset"
      cursor-range
      #(iterative-cursor % cursor-callback))))

(defn- put-items [{:as args :keys [data store-name on-success on-error]}]
  (let [store (-> {:store-names [store-name]
                   :command "readwrite"}
                  get-transaction
                  (.objectStore store-name))]
    (run!
      #(-> (.put store (clj->stored-entry %))
           (set-error-action on-error)
           (set-success-action on-success))
      data)))

(defn put-meta [args]
  (put-items (assoc args :store-name meta-store-name :data [(:data args)])))

(defn put-function [args]
  (put-items (assoc args :store-name function-store-name :data [(:data args)])))

(defn put-fragments [args]
  (put-items (assoc args :store-name fragment-store-name)))

(defn read-all-items [store-name on-complete]
  (let [transaction-params
        {:store-names [store-name]
         :command "readonly"
         :on-complete #(js/console.info "Cursor completed")
         :on-error #(js/console.error "Cursor failed: " %)}
        result (atom [])]
    (-> (get-transaction transaction-params)
        (.objectStore store-name)
        .openCursor
        (set-success-action #(read-all-cursor % result on-complete)))))

(defn read-all-tables [on-complete]
  (read-all-items meta-store-name on-complete))

(defn read-all-functions [on-complete]
  (read-all-items function-store-name on-complete))

(defn count-fragments [{:keys [object-id on-success]}]
  (let [transaction-params
        {:store-names [fragment-store-name]
         :command "readonly"
         :on-complete #(js/console.info "Fragments counted")
         :on-error #(js/console.error "Fragment count failed: " %)}
        request 
        (-> (get-transaction transaction-params)
            (.objectStore fragment-store-name)
            (.index "object")
            (.count object-id))]
    (set! (.-onsuccess request) #(on-success (.-result request)))))

(defn- fetch-object [{:keys [store-name object-key on-complete on-error]}]
  (let [transaction-params
        {:store-names [store-name]
         :command "readonly"
         :on-complete #(js/console.info "Fetched:" store-name object-key)
         :on-error #(js/console.error "Fetch failed:" store-name object-key %)}
        request
        (-> (get-transaction transaction-params)
            (.objectStore store-name)
            (.get object-key))]
    (set!
      (.-onsuccess request)
      #(on-complete (some-> request .-result stored-entry->clj)))
    (set! (.-onerror request) on-error)))


(defn fetch-meta [{:as args :keys [table-id]}]
  (fetch-object
    (assoc args :object-key table-id :store-name meta-store-name)))

(defn fetch-function [{:as args :keys [function-id]}]
  (fetch-object
    (assoc args :object-key function-id :store-name function-store-name)))

(defn- delete-object [{:keys [store-name object-key on-complete]}]
  (let [store-names [store-name fragment-store-name]
        transaction-params
        {:store-names store-names
         :command "readwrite"
         :on-complete #(js/console.info "Deletion transaction completed " %)
         :on-error #(js/console.error "Data deletion failed: " %)}]
    (doto (get-transaction transaction-params)
      (-> (.objectStore store-name)
          (.delete object-key))
      (-> (.objectStore fragment-store-name)
          (.index "object")
          (.openCursor (.only (idb-key-range) object-key))
          (set-success-action #(delete-all-cursor % on-complete))))))

(defn delete-table [{:keys [table-id on-complete]}]
  (delete-object {:store-name meta-store-name
                  :object-key table-id
                  :on-complete on-complete}))

(defn delete-function [{:keys [function-id on-complete]}]
  (delete-object {:store-name function-store-name
                  :object-key function-id
                  :on-complete on-complete}))

(defn delete-fragments [{:keys [object-id on-success]}]
  (let [store-names [fragment-store-name]
        transaction-params {:store-names store-names
                            :command "readwrite"}]
    (doto (get-transaction transaction-params)
      (-> (.objectStore fragment-store-name)
          (.index "object")
          (.openCursor (.only (idb-key-range) object-id))
          (set-success-action #(delete-all-cursor % on-success))))))
