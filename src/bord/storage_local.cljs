(ns bord.storage-local)

(defonce loaded-db (atom nil))
(def db-name "bord-default")
(def db-version 12)

(defn- set-onerror [request callback]
  (set! (.-onerror request) callback)
  request)

(defn- set-onsuccess [request callback]
  (set! (.-onsuccess request) callback)
  request)

(defn- set-oncomplete [request callback]
  (set! (.-oncomplete request) callback)
  request)

(defn- indexed-db []
  (if (-> js/self .-document undefined?)
    js/self.indexedDB
    js/window.indexedDB))

(defn- idb-key-range []
  (if (-> js/self .-document undefined?)
    js/self.IDBKeyRange
    js/window.IDBKeyRange))

(defn- db-init-onsuccess [request callback]
  (reset! loaded-db (.-result request))
  (callback))

(defn- create-index [store config]
  (.createIndex store (:name config) (:key-path config) (clj->js {"unique" (:unique config)})))

(defn- init-store [db config]
  (let [store (.createObjectStore
                db
                (:store-name config)
                (clj->js {"keyPath" (:key-path config)}))]
    (run! #(create-index store %) (:indices config))))

(defn- db-init-onupgradeneeded [event db-request stores on-success on-error]
  (let [transaction (.-transaction db-request)
        db (.. event -target -result)]
    (set-oncomplete transaction on-success)
    (set-onerror db on-error)
    (run! #(init-store db %) stores)
    (reset! loaded-db db)))

(defn db-init [{:keys [on-success on-error stores]}]
  (let [db-request (.open (indexed-db) db-name db-version)]
    (set!
      (.-onupgradeneeded db-request)
      #(db-init-onupgradeneeded % db-request stores on-success on-error))
    (set-onsuccess db-request #(db-init-onsuccess db-request on-success))
    (set-onerror db-request on-error)))

(defn- read-all-cursor [event result on-complete]
  (if-let [cursor (.. event -target -result)]
    (do
      (swap! result conj (.-value cursor))
      (.continue cursor))
    (on-complete @result)))

(defn- delete-all-cursor [event on-complete]
  (if-let [cursor (.. event -target -result)]
    (do
      (.delete cursor)
      (.continue cursor))
    (on-complete)))

(defn- iterative-cursor [event callback]
  (if-let [cursor (.. event -target -result)]
    (when (callback (.-value cursor))
      (.continue cursor))
    (callback nil)))

(defn- get-transaction
  [{:keys [store-name command on-complete on-error]
    :or {command "readonly"
         on-complete #()
         on-error #(js/console.error "transaction error:" %)}}]
  (-> (.transaction @loaded-db [store-name] command)
      (set-oncomplete on-complete)
      (set-onerror on-error)))

(defn read-all-items [store-name on-complete]
  (let [result (atom [])]
    (-> {:store-name store-name}
        (get-transaction)
        (.objectStore store-name)
        (.openCursor)
        (set-onsuccess #(read-all-cursor % result on-complete)))))

(defn read-items [{:keys [store-name index start end cursor-callback]}]
  (let [cursor-bound (.bound (idb-key-range)
                             (clj->js start) (clj->js end)
                             false true)]
    (-> {:store-name store-name}
        (get-transaction) 
        (.objectStore store-name)
        (.index index)
        (.openCursor cursor-bound)
        (set-onsuccess #(iterative-cursor % cursor-callback)))))

(defn read-item [{:keys [store-name item-key on-success on-error]}]
  (let [request
        (-> {:store-name store-name}
            (get-transaction)
            (.objectStore store-name)
            (.get item-key))]
    (set-onsuccess request #(on-success (.-result request)))
    (set-onerror request on-error)))


(defn put-items [{:as args :keys [data store-name on-success on-error]}]
  (let [object-store (-> {:store-name store-name
                          :command "readwrite"}
                         get-transaction
                         (.objectStore store-name))]
    (run!
      #(-> object-store 
           (.put %)
           (set-onerror on-error)
           (set-onsuccess on-success))
      data)))

(defn count-items [{:keys [index value store-name on-success]}]
  (let [request 
        (-> {:store-name store-name}
            (get-transaction)
            (.objectStore store-name)
            (.index index)
            (.count value))]
    (set-onsuccess request #(on-success (.-result request)))))

(defn delete-items [{:keys [index value store-name on-success]}]
  (-> {:store-name store-name
       :command "readwrite"}
      (get-transaction)
      (.objectStore store-name)
      (.index index)
      (.openCursor (.only (idb-key-range) store-name))
      (set-onsuccess #(delete-all-cursor % on-success))))

(defn delete-item [{:keys [store-name item-key on-complete]}]
  (-> {:store-name store-name
       :command "readwrite"}
      (get-transaction)
      (.objectStore store-name)
      (.delete item-key)))
