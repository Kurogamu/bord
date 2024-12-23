(ns bord.data
  (:require 
    [reagent.core :as r]))

(defonce loaded-db (r/atom nil))
(def db-name "bord-default")
(def db-version 7)
(def meta-store-name "table-meta")
(def fragment-store-name "table-fragment")
(def column-store-name "table-column")

(defn init-table-meta-store [db]
  (let [store (.createObjectStore
                db meta-store-name #js {"keyPath" "tableId"})]
    (.createIndex store "name" "name" #js {"unique" false})
    (.createIndex store "created_at" "created_at" #js {"unique" false})
    (.createIndex store "updated_at" "updated_at" #js {"unique" false})
    (.createIndex store "count" "count" #js {"unique" false})))

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
    (.createIndex store "row" "row" #js {"unique" false})))

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

(defn get-transaction [{:keys [store-names command on-complete on-error]}]
  (let [transaction (.transaction @loaded-db store-names command)]
    (set! (.-oncomplete transaction) on-complete)
    (set! (.-onerror transaction) on-error)
    transaction))

(defn add-table [{:as args :keys [data on-complete on-error]}]
  (let [store-names [meta-store-name column-store-name fragment-store-name]
        transaction (-> args
                        (select-keys [:on-complete :on-error])
                        (assoc :store-names store-names :command "readwrite")
                        get-transaction)
        meta-store (.objectStore transaction meta-store-name)
        column-store (.objectStore transaction column-store-name)
        row-store (.objectStore transaction fragment-store-name)]
    (doseq [column (:columns data)] (.add column-store (clj->js column)))
    (.add meta-store (clj->js (:meta data)))
    (.add row-store (clj->js (:rows data)))))

(defn update-table [{:as args :keys [data on-complete on-error]}]
  (add-table args))

(defn get-db-cursor [store-name]
  (-> (get-transaction {:store-names [store-name]
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
