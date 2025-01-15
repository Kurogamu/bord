(ns bord.core
  (:require
    [bord.state :refer [app-state emit]]
    [bord.table-editor :refer [load-table-editor table-editor]]
    [cljs.core.async :refer [go go-loop chan put!]]
    [bord.data :refer [db-init read-all-tables]]
    [reagent.core :as r]
    [reagent.dom :as d]
    ["react" :as react]
    [cljs.reader :refer [read-string]]
    [clojure.string :as str]))

;; -------------------------
;; Task

(defn read-tables [state]
  (emit [:set-tables-loading true])
  (read-all-tables #(emit [:set-tables %])))

;; -------------------------
;; View

(defn topmenu [state]
  [:div {:class "top-menu"}
   [:div {:class "left-group"}
    [:button {:class "btn add-table-btn"
              :on-click #(load-table-editor :new)}
     "Add table"]]
   [:div {:class "center-group"}
    [:div {:class "title"} "Bord"]]
   [:div {:class "right-group"}]])

(defn render-cell [{:keys [data]}]
  (if (some? data)
    [:td (str data)]
    [:td {:class "blank"} "Blank"]))

(defn render-table-data [table]
  [:div {:class "table-content"}
    [:table
     [:tr
      (for [column-id (:sort-columns table)]
        [:th {:key column-id}
         (-> table (get-in [:columns column-id :name]) str)])]
     (for [[row-index row-data] (map-indexed vector (:data-preview table))]
       [:tr {:key row-index}
        (for [column-id (:sort-columns table)]
         [render-cell {:key column-id
                       :data (get row-data column-id)}])])]])

(defn data-table [data]
  [:div {:class "table-container"}
   (for [entry data]
     [:div {:key (:id entry)
            :class "table"
            :on-click #(load-table-editor entry)}
      [:div {:class "table-header"}
       (:name entry)]
      [:div {:class "table-subheader"}
       (.toLocaleString (js/Date. (:updated entry)))]
      (render-table-data entry)])])

(defn main [state]
  [:div {:class "main"}
   (if (count (:tables @app-state)) (data-table (:tables @app-state)))
   (if (:tables-loading @app-state) [:div "loading tables..."])
   (if (some? (:table-editor @app-state))
     [table-editor])])

(defn app-root [state]
  [:div {:class "app-root"}
   [topmenu state]
   [main state]])

;; -------------------------
;; Handler

(def keydown-ch (chan))
(js/document.addEventListener "keydown" #(put! keydown-ch (.-key %)))

(defn keydown-handler []
  (go-loop [last-pressed nil]
    (let [pressed-key (<! keydown-ch)]
      (if (not= last-pressed pressed-key)
        (case pressed-key
          "t" (if (nil? (:table-editor @app-state))
                (load-table-editor :new))
          "Escape" (emit [:close-editor nil])
          nil))
      (recur pressed-key))))


;; -------------------------
;; Initialize app

(defn mount-root [] (d/render [app-root] (.getElementById js/document "app")))

(defn init-db []
  (let [on-success #(read-tables app-state)
        on-error #(js/console.error "Failed to init db!" %)]
    (db-init {:on-success on-success :on-error on-error})))

(defn init []
  (mount-root)
  (init-db)
  (keydown-handler))

(js/document.addEventListener "DOMContentLoaded" init)
