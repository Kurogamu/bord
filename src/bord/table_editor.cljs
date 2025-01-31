(ns bord.table-editor
  (:require
    [reagent.core :as r]
    [clojure.string :refer [blank?]]
    [bord.state :refer [app-state emit]]
    [bord.data :refer [fetch-fragment put-meta put-fragment delete-table]]
    [cljs.core.async :refer [go timeout]]
    ["react" :as react]))

;; -------------------------
;; Model

(def editor-cursor (r/cursor app-state [:table-editor]))

(defn init-column-data []
  {:id (js/crypto.randomUUID)
   :name ""
    :type :string})

(defn init-fragment-data [table-id]
  {:id (js/crypto.randomUUID)
   :table-id table-id
   :first-row 0
   :last-row 0
   :data [{}]})

(defn init-table-data []
  (let [column (init-column-data)]
    {:id (js/crypto.randomUUID)
     :name ""
     :created (js/Date.now)
     :updated (js/Date.now)
     :columns {(:id column) column}
     :sort-columns [(:id column)]
     :data-preview []
     :count 1}))


;; -------------------------
;; Update

(defn calculate-move-cell [state direction]
  (let [[row column-id] (get-in state [:table-editor :active-cell])
        sort-columns (get-in state [:table-editor :meta :sort-columns])
        column-index (.indexOf sort-columns column-id)
        last-row (-> (get-in state [:table-editor :fragment :data]) count dec)
        last-column (-> (get-in state [:table-editor :meta :columns]) count dec)]
    (case direction
      :down (if (< row last-row)
                [(inc row) column-id]
                [row column-id])
      :prev (if (> column-index 0)
              [row (nth sort-columns (dec column-index))]
              [(dec row) (last sort-columns)])
      :next (if (< column-index last-column)
              [row (nth sort-columns (inc column-index))]
              [(inc row) (first sort-columns)])
      [row column-id])))

(defn update-data-preview [state]
  (let [preview-row-count (min (get-in state [:table-editor :meta :count]) 5)
        preview-rows (subvec
                       (get-in state [:table-editor :fragment :data])
                       0
                       preview-row-count)]
    (assoc-in state [:table-editor :meta :data-preview] preview-rows)))

(defn handler [state [event value]]
  (case event
    :set-updated (assoc-in state [:table-editor :meta :updated] value)
    :set-table-name (assoc-in state [:table-editor :meta :name] value)
    :set-column-name
    (let [[id name] value]
      (assoc-in state [:table-editor :meta :columns id :name] name))

    :set-column-type
    (let [[id type] value]
      (assoc-in state [:table-editor :meta :columns id :type] type))

    :add-column
    (let [new-column (init-column-data)]
      (-> state
          (assoc-in [:table-editor :meta :columns (:id new-column)] new-column)
          (update-in [:table-editor :meta :sort-columns] #(conj % (:id new-column)))))

    :edit-cell
    (let [cell-path (concat
                      [:table-editor :fragment :data]
                      (get-in state [:table-editor :active-cell]))]
      (-> state
          (assoc-in cell-path value)
          update-data-preview))

    :unset-active-cell (assoc-in state [:table-editor :active-cell] nil)
    :move-active-cell (assoc-in state
                                [:table-editor :active-cell]
                                (calculate-move-cell state value))

    :set-active-cell (assoc-in state [:table-editor :active-cell] value)
    :set-fragment (assoc-in state [:table-editor :fragment] value)
    :add-row
    (-> state
        (update-in [:table-editor :fragment :data] #(conj % {}))
        (update-in [:table-editor :meta :count] inc)
        update-data-preview)

    :init-closing (assoc-in state [:table-editor :closing] true)

    state))

;; -------------------------
;; Task

(defn init-fragment []
  (let [new-fragment (init-fragment-data (get-in @editor-cursor [:meta :id]))
        success-callback #(emit [:set-fragment new-fragment] handler)
        error-callback #(js/console.error "Failed to create fragment" %)]
    (put-fragment {:data new-fragment
                   :on-complete success-callback
                   :on-error error-callback})))

(defn init-table [on-complete]
  (let [new-table (init-table-data)
        success-callback (fn []
                           (emit [:set-editor-table new-table])
                           (on-complete))
        error-callback #(js/console.error "Failed to create table!" %)]
    (put-meta {:data new-table
               :on-complete success-callback
               :on-error error-callback})))

(defn setup-new-table []
  (init-table init-fragment))

(defn load-existing-table [table]
  (emit [:open-editor-table table])
  (fetch-fragment {:table-id (:id table)
                   :row-number 0
                   :on-complete #(emit [:set-fragment %] handler)}))

(defn load-table-editor [table]
  (if (= table :new)
    (setup-new-table)
    (load-existing-table table)))

(def store-meta-queue (r/atom 0))

(defn store-meta []
  (reset! store-meta-queue 0)
  (let [data (:meta @editor-cursor)
        success-callback (fn []
                           (emit [:set-table data])
                           (js/console.info "Data saved"))
        error-callback #(js/console.error "Failed to create table!" %)]
    (put-meta {:data data
               :on-complete success-callback
               :on-error error-callback})))

(defn debounce-store-meta []
  (go
    (swap! store-meta-queue inc)
    (<! (timeout 2000))
    (if (> @store-meta-queue 1)
      (swap! store-meta-queue dec)
      (store-meta))))

(defn emit-edit-meta [msg]
  (emit msg handler)
  (emit [:set-updated (js/Date.now)] handler)
  (debounce-store-meta))

(def store-fragment-queue (r/atom 0))

(defn store-fragment []
  (reset! store-fragment-queue 0)
  (let [success-callback #(js/console.info "Fragment saved")
        error-callback #(js/console.error "Failed to create table!" %)]
    (put-fragment {:data (:fragment @editor-cursor)
                  :on-complete success-callback
                  :on-error error-callback})))

(defn debounce-store-fragment []
  (go
    (swap! store-fragment-queue inc)
    (<! (timeout 2000))
    (if (> @store-fragment-queue 1)
      (swap! store-fragment-queue dec)
      (store-fragment))))

(defn emit-edit-fragment [msg]
  (emit msg handler)
  (emit [:set-updated (js/Date.now)] handler)
  (debounce-store-fragment))

(defn close-modal []
  (js/window.addEventListener
    "animationend" 
    #(if (= (.-animationName %) "slide-out") (emit [:close-editor nil]))
    #js {:once true})
  (emit [:init-closing nil] handler))


(defn close-table-editor []
  (store-meta)
  (store-fragment)
  (close-modal))

(defn delete []
  (let [table (:meta @editor-cursor)
        delete-callback #(emit [:delete-table table])]
    (delete-table {:table-id (:id table)
                   :on-complete delete-callback})
    (close-modal)))

;; -------------------------
;; View
(defn editor-name []
  [:div {:class "modal-section name-editor"}
   [:h3 "Name"]
   [:div {:class "input-wrapper"}
    [:input
     {:type "text"
      :value (get-in @editor-cursor [:meta :name])
      :auto-focus true
      :placeholder "New table name"
      :on-change #(emit-edit-meta [:set-table-name (.. % -target -value)])}]]])

(defn editor-column [column-id]
  [:div {:key column-id :class "column-editor"}
   [:div {:class "input-wrapper"}
    [:input
     {:type "text"
      :value (get-in @editor-cursor [:meta :columns column-id :name])
      :auto-focus true
      :placeholder "New column name"
      :on-change #(emit-edit-meta [:set-column-name [column-id (.. % -target -value)]])}]]
   [:div {:class "input-wrapper"}
    [:select
     {:value (get-in @editor-cursor [:meta :columns column-id :type])
      :on-change #(emit-edit-meta [:set-column-type [column-id (.. % -target -value)]])}
     [:option { :value :string } "Text"]
     [:option { :value :number } "Number"]]]])

(defn editor-columns []
  [:div {:class "modal-section column-set-editor"}
   [:h3 "Columns"]
   (doall (for [column-id (get-in @editor-cursor [:meta :sort-columns])]
            (editor-column column-id)))
   [:button {:class "btn add-column-btn"
             :on-click #(emit-edit-meta [:add-column nil])}
    "Add column"]])

(defn cell-editor [column-type]
  (let [value (get-in
                @editor-cursor
                (concat [:fragment :data] (:active-cell @editor-cursor)))
        on-change (fn [event]
                    (as-> (.. event -target -value) v
                      (if (= :number (keyword column-type))
                        (re-find #"[0-9]*\.?[0-9]*" v)
                        v)
                      (emit-edit-fragment [:edit-cell v])))]
    [:input
     {:type (if (= column-type :number) "number" "text")
      :auto-focus true
      :value value
      :placeholder "New value"
      :on-change on-change
      :on-blur #(emit [:unset-active-cell nil] handler)
      :on-key-down (fn [e]
                     (case (.-key e)
                       "Enter" (do
                                 (.preventDefault e)
                                 (emit [:move-active-cell :down] handler))
                       "Tab" (do
                               (.preventDefault e)
                               (if (.-shiftKey e)
                                 (emit [:move-active-cell :prev] handler)
                                 (emit [:move-active-cell :next] handler)))
                       nil))}]))

(defn editor-cell [cell-key data]
  (let [activate-cell #(emit [:set-active-cell cell-key] handler)]
    (cond
      (= cell-key (:active-cell @editor-cursor))
        [:td {:key cell-key :class "active"}
         (cell-editor (get-in @editor-cursor [:meta :columns (second cell-key) :type]))]
      (blank? data)
        [:td {:key cell-key :class "blank" :on-click activate-cell} "Blank"]
      :else
        [:td {:key cell-key :on-click activate-cell} (str data)])))

(defn editor-data []
  [:div {:class "modal-section table-editor"}
   [:h3 "Data"]
   [:div {:class "table-wrapper"}
    [:table
     [:tr
      (doall
        (for [column-id (get-in @editor-cursor [:meta :sort-columns])]
          (let [name (get-in @editor-cursor [:meta :columns column-id :name])]
            (if (blank? name)
              [:th {:key column-id :class "blank"} "Blank"]
              [:th {:key column-id} name]))))]
     (doall
       (for [[row-index row-data] (map-indexed vector (get-in @editor-cursor [:fragment :data]))]
         [:tr {:key row-index}
          (doall
            (for [column-id (get-in @editor-cursor [:meta :sort-columns])]
              (editor-cell [row-index column-id] (get row-data column-id))))]))]
    [:button {:class "btn add-row-btn"
              :on-click #(emit [:add-row nil] handler)}
     "Add row"]]])

(defn table-editor []
  [:div {:class (if (:closing @editor-cursor)
                  "modal modal-editor modal-editor-closing"
                  "modal modal-editor")}
   [:div {:class "modal-header"}
    [:div {:class "modal-title"} "Table Editor"]
    [:div {:class "modal-menu btn-group"}
    [:button {:class "delete"
              :on-click delete}
     "Delete"]
    [:button {:class "close"
              :on-click close-table-editor}
     "Close"]]]
   [:div {:class "modal-body"}
    [:div {:class "table-editor"}
     [editor-name]
     [editor-columns]
     [editor-data]]]])
