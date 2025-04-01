(ns bord.core
  (:require
    [bord.state :refer [app-state emit function-outputs]]
    [bord.table-editor :refer [load-table-editor table-editor]]
    [bord.function-editor :refer [load-function-editor function-editor]]
    [cljs.core.async :refer [go go-loop chan put!]]
    [bord.data :refer [db-init read-all-tables read-all-functions]]
    [reagent.core :as r]
    [reagent.dom :as d]
    ["react" :as react]
    [cljs.reader :refer [read-string]]
    [clojure.string :as str]))

;; -------------------------
;; Task

(defn read-tables []
  (emit [:set-tables-loading true])
  (read-all-tables #(emit [:set-tables %])))

(defn read-functions []
  (emit [:set-functions-loading true])
  (read-all-functions #(emit [:set-functions %])))

(defn read-db []
  (read-tables)
  (read-functions))

;; -------------------------
;; View

(defn topmenu [state]
  [:div
   {:class "top-menu"}
   [:div
    {:class "center-group"}
    [:div {:class "title"} "bord"]]])

(defn render-cell [{:keys [data]}]
  (if (some? data)
    [:td (str data)]
    [:td {:class "blank"} "Blank"]))

(defn render-table-data [table]
  [:div {:class "card-content table"}
    [:table
     [:tr
      (for [column-id (:sort-columns table)]
        [:th
         {:key column-id}
         (-> table (get-in [:columns column-id :name]) str)])]
     (for [[row-index row-data]
           (map-indexed vector (:data-preview table))]
       [:tr
        {:key row-index}
        (for [column-id (:sort-columns table)]
         [render-cell
          {:key column-id :data (get row-data column-id)}])])]])

(defn render-function-preview [function]
  [:div {:class "card-content table"}
   [:table
    [:tr
     (for [output (function-outputs function)]
       [:th {:key (:id output)} (:name output)])]
    (if (seq (:preview function))
      (for [[index result-row]
            (map-indexed vector (:preview function))]
        [:tr {:key index}
         (for [[id value] result-row]
           [:td {:key id} (or (str value) "Blank")])]))]])

(defn function-card [function]
  [:div
   {:key (:id function)
    :class "card card-function"
    :on-click #(load-function-editor function)}
   [:div {:class "card-header"} (:name function)]
   [:div
    {:class "card-subheader"}
    (.toLocaleString (js/Date. (:updated function)))]
   (render-function-preview function)])

(defn table-card [table]
  [:div
   {:class "card card-table"
    :on-click #(load-table-editor table)}
   [:div {:class "card-header"} "Data"]
   (render-table-data table)])

(defn table-collection [table]
  (let [table-functions (filter
                          #(= (:id table) (:source %))
                          (vals (:functions @app-state)))]
    [:div
     {:key (:id table)
      :class "table-container"}
     [:div
      {:class "table-container-header"
       :on-click #(load-table-editor table)}
      (:name table)]
     [:div
      {:class "table-container-subheader"}
      (-> table :updated js/Date. .toLocaleString)]
     [:div
      {:class "table-container-cards"}
      (table-card table)
      (doall (map function-card table-functions))
      [:div
       {:class "button-container"}
       [:button
        {:class "btn description-btn"
         :on-click #(load-function-editor {:source (:id table)})}
        [:div
         {:class "description-btn-header"}
         "Add Function"]
        [:div
         {:class "description-btn-description"}
         "Apply calculations on table data"]]]]]))

(defn main-container [data]
  [:div {:class "main-container"}
   (doall (map table-collection (vals data)))
   [:div
    {:class "button-container"}
    [:button
     {:class "btn add-table-btn"
      :on-click #(load-table-editor :new)}
     [:div {:class "description-btn-header"} "Add Table"]
     [:div {:class "description-btn-description"} "Create new table"]]]])

(defn main [state]
  [:div {:class "main"}
   (if (count (:tables @app-state))
     (main-container (:tables @app-state)))
   (if (:tables-loading @app-state)
     [:div "loading tables..."])
   (if (some? (:table-editor @app-state))
     [table-editor])
   (if (some? (:function-editor @app-state))
     [function-editor])])

(defn app-root [state]
  [:div {:class "app-root"}
   [topmenu state]
   [main state]])

;; -------------------------
;; Handlers

(def keydown-ch (chan))
(js/document.addEventListener "keydown" #(put! keydown-ch (.-key %)))

(defn keydown-handler []
  (go-loop [last-pressed nil]
    (let [pressed-key (<! keydown-ch)]
      (if (not= last-pressed pressed-key)
        (case pressed-key
          "t"
          (if (and (nil? (:table-editor @app-state))
                   (nil? (:function-editor @app-state)))
            (load-table-editor :new))

          "Escape" (emit [:close-editor nil])
          nil))
      (recur pressed-key))))


;; -------------------------
;; Initialize app

(defn mount-root [] (d/render [app-root] (.getElementById js/document "app")))

(defn init-db []
  (let [on-success read-db
        on-error #(js/console.error "Failed to init db!" %)]
    (db-init {:on-success on-success :on-error on-error})))

(defn init-worker []
  (let [worker (js/Worker. "js/worker.js")]
    (.. worker
        (addEventListener "message"
                          (fn [e] (js/console.log "hello from worker " e))))
    (.. worker (postMessage "hello to worker"))))

(defn init []
  (mount-root)
  (init-db)
  (init-worker)
  (keydown-handler))

(js/document.addEventListener "DOMContentLoaded" init)
