(ns bord.core
  (:require
    [bord.common :refer [keycodes]]
    [bord.editor :refer [table-editor table-editor-init-state]]
    [cljs.core.async :refer [go go-loop chan put!]]
    [bord.data :refer [db-init store-table read-all]]
    [reagent.core :as r]
    [reagent.dom :as d]
    ["react" :as react]
    [cljs.reader :refer [read-string]]
    [clojure.string :as str]))

;; -------------------------
;; Model

(def default-state
  {:tables []
   :editor nil
   :tables-loading false})

(defonce app-state (r/atom default-state))

;; -------------------------
;; Update

(defn open-editor [msg state]
  (if (some? (:target msg))
    (table-editor-init-state (:target msg))
    (table-editor-init-state))
  (swap! state assoc :editor (:editor-type msg)))

(defn close-editor [msg state]
  (swap! state assoc :editor nil :editor-target nil))

(defn set-tables-loading [msg state]
  (swap! state assoc :tables-loading msg))

(defn set-tables [msg state]
  (swap! state assoc :tables msg :tables-loading false))

;; -------------------------
;; Task

(defn read-tables [state]
  (let [tables (r/atom [])]
    (set-tables-loading true state)
    (read-all #(set-tables % state))))

(defn save-table [state table-data]
  (close-editor nil state)
  (let [success-callback #(read-tables state)
        error-callback #(js/console.error "Failed to write table!" %)]
    (store-table {:data table-data
                  :on-complete success-callback
                  :on-error error-callback})))

;; -------------------------
;; View

(defn topmenu [state]
  [:div {:class "top-menu"}
   [:div {:class "left-group"}
    [:button {:class "btn add-table-btn"
              :on-click #(open-editor {:editor-type :table} app-state)}
     "Add table"]]
   [:div {:class "center-group"}
    [:div {:class "title"} "Bord"]]
   [:div {:class "right-group"}]])

(defn render-cell [{:keys [column cell]}]
  [:td {:key (:columnId column)}
   (str (or cell "Blank"))])

(defn render-table-data [table]
  [:div {:class "table-content"}
    [:table
     [:tr
      (for [column-id (:sort-columns table)]
       [:th {:key column-id} (str (:name (get (:columns table) column-id)))])]
     (for [[row-index row-data] (map-indexed vector (:rows table))]
       [:tr {:key row-index}
        (for [column-id (:sort-columns table)]
         [render-cell {:cell (get row-data column-id)
                       :column (get (:columns table) column-id)}])])]])

(defn data-table [data]
  [:div {:class "table-container"}
   (for [entry data]
     [:div {:key (:tableId entry)
            :class "table"
            :on-click #(open-editor {:editor-type :table
                                     :target entry}
                                    app-state)}
      [:div {:class "table-header"}
       (:name entry)]
      [:div {:class "table-subheader"}
       (.toLocaleString (js/Date. (:updated entry)))]
      (render-table-data entry)
      ])])

(defn main [state]
  [:div {:class "main"}
   (if (count (:tables @state)) (data-table (:tables @state)))
   (if (:tables-loading @state) [:div "loading tables..."])
   (if (= (:editor @state) :table)
     [table-editor {:on-save #(save-table state %)
                    :on-close #(close-editor nil state)}])])

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
          "t" (if (nil? (:editor @app-state))
                (open-editor {:editor-type :table} app-state))
          "Escape" (close-editor nil app-state)
          nil))
      (recur pressed-key))))


;; -------------------------
;; Initialize app

(defn mount-root [] (d/render [app-root app-state] (.getElementById js/document "app")))

(defn init-db []
  (let [on-success #(read-tables app-state)
        on-error #(js/console.error "Failed to init db!" %)]
    (db-init {:on-success on-success :on-error on-error})))

(defn init []
  (mount-root)
  (init-db)
  (keydown-handler))

(js/document.addEventListener "DOMContentLoaded" init)
