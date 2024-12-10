(ns bord.data
  (:require 
    [reagent.core :as r]))

(defonce loaded-db (r/atom nil))
(def db-name "bord-default")
(def db-version 5)
(def meta-store-name "table-meta")
(def fragment-store-name "table-fragment")
(def column-store-name "table-column")

(defn init-table-meta-store [db]
  (let [store (.createObjectStore
                db meta-store-name #js {"keyPath" "tableId"})]
    (.createIndex store "name" "name" #js {"unique" false})
    (.createIndex store "created_at" "created_at" #js {"unique" false})
    (.createIndex store "updated_at" "updated_at" #js {"unique" false})
    (.createIndex store "count" "count" #js {"unique" false})
    (.createIndex store "columns" "columns" #js {"unique" false})))

(defn init-table-column-store [db]
  (let [store (.createObjectStore
                db column-store-name #js {"keyPath" "columnId"})]
    (.createIndex store "tableId" "tableId" #js {"unique" false})
    (.createIndex store "name" "name" #js {"unique" false})
    (.createIndex store "type" "type" #js {"unique" false})))

(defn init-table-fragment-store [db]
  (let [store (.createObjectStore
                db fragment-store-name #js {"keyPath" "fragmentId"})]
    (.createIndex store "tableId" "tableId" #js {"unique" false})
    (.createIndex store "columnId" "columnId" #js {"unique" false})
    (.createIndex store "row" "row" #js {"unique" false})
    (.createIndex store "data" "data" #js {"unique" false})))

(defn db-init-onsuccess [request callback]
  (reset! loaded-db (.-result request))
  (callback))

(defn db-init-onupgradeneeded [event db-request on-success on-error]
  (let [transaction (.-transaction db-request)
        db (.. event -target -result)]
    (set! (.-oncomplete transaction) on-success)
    (set! (.-onerror db) on-error)
    (init-table-meta-store db)
    (init-table-column-store db)
    (init-table-fragment-store db)
    (reset! loaded-db db)))

(defn db-init [{:keys [on-success on-error]}]
  (let [db-request (js/window.indexedDB.open db-name db-version)]
    (set! (.-onsuccess db-request)
          #(db-init-onsuccess db-request on-success))
    (set! (.-onupgradeneeded db-request)
          #(db-init-onupgradeneeded % db-request on-success on-error))
    (set! (.-onerror db-request) on-error)))

(defn get-transaction [{:keys [store-name command on-complete on-error]}]
    (let [transaction (.transaction @loaded-db #js [store-name] command)]
      (set! (.-oncomplete transaction) on-complete)
      (set! (.-onerror transaction) on-error)
      transaction))

(defn db-put [{:as args :keys [data store-name on-complete on-error]}]
  (-> (get-transaction (assoc args :command "readwrite"))
      (.objectStore store-name)
      (.put (clj->js data))))

(defn db-add [{:as args :keys [data store-name on-complete on-error]}]
  (-> (get-transaction (assoc args :command "readwrite"))
      (.objectStore store-name)
      (.add (clj->js data))))

(defn update-table [{:as args :keys [data on-complete on-error]}]
  (db-put (assoc args :store-name meta-store-name)))

(defn add-table [{:as args :keys [data on-complete on-error]}]
  (let [new-table (assoc data :tableId (js/crypto.randomUUID))]
    (db-add (assoc args :data new-table :store-name meta-store-name))))

(defn add-fragment [{:as args :keys [data on-complete on-error]}]
  (let [new-fragment (assoc data :fragmentId (js/crypto.randomUUID))]
    (db-add (assoc args :data new-fragment :store-name fragment-store-name))))

(defn get-db-cursor [store-name]
  (-> (get-transaction {:store-name store-name
                        :command "readonly"
                        :on-complete #(js/console.info "Cursor completed")
                        :on-error #(js/console.error "Cursor failed: " %)})
      (.objectStore store-name)
      (.openCursor)))

(defn table-read-all [on-complete]
  (let [result (r/atom [])]
    (set! (.-onsuccess (get-db-cursor meta-store-name))
          (fn [event]
            (let [cursor (.. event -target -result)]
              (if (some? cursor)
                (do 
                  (swap! result conj (.-value cursor))
                  (.continue cursor))
                (on-complete @result)))))))
