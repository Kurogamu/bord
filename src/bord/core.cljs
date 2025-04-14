(ns bord.core
  (:require
    [bord.table-view :refer [table-component]]
    [bord.state :refer [app-state emit function-outputs]]
    [bord.table-editor :refer [load-table-editor table-editor]]
    [bord.function-editor :refer [load-function-editor function-editor]]
    [bord.table-uploader :refer [table-uploader]]
    [bord.task-monitor :refer [task-summary task-view-popup]]
    [bord.worker-handler :refer [init-workers]]
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

(defn load-table-uploader []
  (emit [:set-table-upload-dialog true]))

;; -------------------------
;; View

(defn topmenu []
  [:div.top-menu
   [:div.left-group]
   [:div.center-group [:div.title "bord"]]
   [:div.right-group
    [task-summary]]])

(defn render-table-data [table]
  [:div {:class "card-content"}
   [table-component {:sort-columns (:sort-columns table)
                     :columns (:columns table)
                     :data-rows (:data-preview table)}]])


(defn render-function-preview [function]
  [:div {:class "card-content"}
   [table-component {:sort-columns (:outputs function)
                     :columns (function-outputs @app-state function)
                     :data-rows (:preview function)}]])

(defn function-card [function]
  [:div
   {:key (:id function)
    :class "card card-function"
    :on-click #(load-function-editor function)}
   [:div.card-header (:name function)]
   [:div.card-subheader
    (.toLocaleString (js/Date. (:updated function)))]
   (case (:state function)
     :deleting [:div.deleting "Deleting function..."]
     :processing [:div.processing "Processing function..."]
     (render-function-preview function))])

(defn table-card [table]
  [:div
   {:class "card card-table"
    :on-click #(load-table-editor table)}
   [:div.card-header "Data"]
   [:div.card-subheader
    (.toLocaleString (js/Date. (:updated table)))]
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
     [:div.table-container-subheader
      (str (:count table) " rows")]
     [:div.table-container-cards
      (table-card table)
      (doall (map function-card table-functions))
      [:div.button-container
       [:button
        {:class "btn description-btn"
         :on-click #(load-function-editor {:source (:id table)})}
        [:div.description-btn-header "Add Function"]
        [:div.description-btn-description
         "Apply calculations on table data"]]]]]))

(defn main-container [data]
  [:div.main-container
   (doall (map table-collection (vals data)))
   [:div.button-container
    [:button
     {:class "btn add-table-btn"
      :on-click #(load-table-editor :new)}
     [:div.description-btn-header "Add Table"]
     [:div.description-btn-description "Create new table"]]
    [:button
     {:class "btn upload-table-btn"
      :on-click load-table-uploader}
     [:div.description-btn-header "Upload Table"]
     [:div.description-btn-description "Create table from file"]]]])

(defn main []
  [:div.main
   (if (count (:tables @app-state))
     (main-container (:tables @app-state)))
   (if (:tables-loading @app-state)
     [:div "loading tables..."])])

(defn app-root []
  [:div.app-root
   [topmenu]
   [main]
   (if (some? (:table-editor @app-state))
     [table-editor])
   (if (some? (:function-editor @app-state))
     [function-editor])
   (if (some? (:table-uploader @app-state))
     [table-uploader])
   (if (some? (:task-viewer @app-state))
     [task-view-popup])])

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

(defn init-app []
  (js/console.info "Initializing app..")
  (mount-root)
  (init-db)
  (init-workers)
  (keydown-handler))

(defn entry-point []
  (js/document.addEventListener "DOMContentLoaded" init-app))
