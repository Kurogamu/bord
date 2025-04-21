(ns bord.data
  (:require
    [bord.storage-local :as db]
    [clojure.walk :refer [keywordize-keys stringify-keys postwalk]]))

(def stores
  {:table
   {:store-name "table"
    :key-path "id"
    :indices [{:name "name"
               :key-path "name"
               :unique false}
              {:name "created"
               :key-path "created_at"
               :unique false}
              {:name "updated"
               :key-path "updated_at"
               :unique false}]}
   :function
   {:store-name "function"
    :key-path "id"
    :indices [{:name "source"
               :key-path "source_id"
               :unique false}]}
   :fragment
   {:store-name "fragment"
    :key-path "id"
    :indices [{:name "object"
               :key-path "object_id"
               :unique false}
              {:name "row"
               :key-path ["object_id" "last_row"]
               :unique true}
              {:name "offset"
               :key-path ["object_id" "offset"]
               :unique true}]}})

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

(defn init-storage [args]
  (db/db-init (assoc args :stores (vals stores))))

(defn read-row-fragments [{:keys [object-id start-row cursor-callback]}]
  (db/read-items
    {:store-name (:store-name (:fragment stores))
     :index "row"
     :start [object-id start-row]
     :end [object-id js/Infinity]
     :cursor-callback #(cursor-callback (some-> % stored-entry->clj))}))

(defn read-fragments [{:keys [object-id offset limit cursor-callback]}]
  (db/read-items
    {:store-name (:store-name (:fragment stores))
     :index "offset"
     :start [object-id offset]
     :end [object-id (+ offset limit)]
     :cursor-callback #(cursor-callback (some-> % stored-entry->clj))}))

(defn count-fragments [{:keys [object-id on-success]}]
  (db/count-items {:index "object"
                   :store-name (:store-name (:fragment stores))
                   :value object-id
                   :on-success on-success}))

(defn put-fragments [{:keys [data on-success on-error]}]
  (db/put-items
    {:store-name (:store-name (:fragment stores))
     :data (map clj->stored-entry data)
     :on-success on-success
     :on-error on-error}))

(defn delete-fragments [{:keys [object-id on-success]}]
  (db/delete-items {:index "object"
                    :store-name (:store-name (:fragment stores))
                    :value object-id
                    :on-success on-success}))

(defn read-all-tables [on-complete]
  (db/read-all-items (:store-name (:table stores)) #(on-complete (stored->clj %))))

(defn fetch-meta [{:keys [table-id on-complete on-error]}]
  (db/read-item
    {:item-key table-id
     :store-name (:store-name (:table stores))
     :on-success #(on-complete (some-> % stored-entry->clj))
     :on-error on-error}))

(defn put-meta [{:keys [data on-success on-error]}]
  (db/put-items
    {:store-name (:store-name (:table stores))
     :data [(clj->stored-entry data)]
     :on-success on-success
     :on-error on-error}))

(defn delete-table [{:keys [table-id on-complete]}]
  (letfn [(on-fragment-delete []
            (db/delete-item {:store-name (:store-name (:table stores))
                             :item-key table-id
                             :on-complete on-complete}))]
    (delete-fragments
      {:object-id table-id
       :on-success on-fragment-delete})))

(defn fetch-function [{:keys [function-id on-complete on-error]}]
  (db/read-item
    {:item-key function-id
     :store-name (:store-name (:function stores))
     :on-success #(on-complete (some-> % stored-entry->clj))
     :on-error on-error}))

(defn put-function [{:keys [data on-success on-error]}]
  (db/put-items
    {:store-name (:store-name (:function stores))
     :data [(clj->stored-entry data)]
     :on-success on-success
     :on-error on-error}))

(defn delete-function [{:keys [function-id on-complete]}]
  (letfn [(on-fragment-delete []
            (db/delete-item {:store-name (:store-name (:function stores))
                             :item-key function-id
                             :on-complete on-complete}))]
    (delete-fragments
      {:object-id function-id
       :on-success on-fragment-delete})))

(defn read-all-functions [on-complete]
  (db/read-all-items (:store-name (:function stores)) #(on-complete (stored->clj %))))
