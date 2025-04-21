(ns bord.table-editor
  (:require
    [reagent.core :as r]
    [clojure.string :refer [blank?]]
    [bord.table-view :refer [table-component]]
    [bord.state :refer [app-state emit]]
    [bord.data :as data]
    [bord.common :refer [find-first-i]]
    [cljs.core.async :refer [go timeout]]
    ["react" :as react]))

;; Delay after last interaction before saving
(def debounce-timeout 2000)

;; -------------------------
;; Model

(def editor-cursor (r/cursor app-state [:table-editor]))

(defn init-column-data []
  {:id (js/crypto.randomUUID)
   :name ""
   :type :string})

(defn init-fragment-data [table-id]
  {:id (js/crypto.randomUUID)
   :object-id table-id
   :first-row 0
   :last-row 1
   :offset 0
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
  (let [[_ row column-id] (get-in state [:table-editor :active-cell])
        sort-columns (get-in state [:table-editor :meta :sort-columns])
        column-index (find-first-i sort-columns column-id)

        [row-offset _ limit] (get-in state [:table-editor :view-rows])
        last-row (+ row-offset limit)

        last-column
        (-> (get-in state [:table-editor :meta :columns])
            count
            dec)]
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
  (let [preview-rows
        (-> (get-in state [:table-editor :meta :count])
            (min 5)
            (take (get-in state [:table-editor :fragments 0 :data])))]
    (assoc-in state [:table-editor :meta :data-preview] preview-rows)))

(defn handler [state [event value]]
  (let [meta-path (fn [& args] (concat [:table-editor :meta] args))]
    (case event
      :set-updated (assoc-in state (meta-path :updated) value)
      :set-table-name (assoc-in state (meta-path :name) value)

      :set-column-name
      (let [[id name] value]
        (assoc-in state (meta-path :columns id :name) name))

      :set-column-type
      (let [[id type] value]
        (assoc-in state (meta-path :columns id :type) type))

      :add-column
      (let [new-column (init-column-data)]
        (-> state
            (assoc-in (meta-path :columns (:id new-column)) new-column)
            (update-in (meta-path :sort-columns) conj (:id new-column))))

      :edit-cell
      (let [[fragment-index row-index column-id]
            (get-in state [:table-editor :active-cell])
            cell-path [:table-editor
                       :fragments
                       fragment-index
                       :data
                       row-index
                       column-id]]
        (-> (assoc-in state cell-path value)
            update-data-preview))

      :set-active-cell (assoc-in state [:table-editor :active-cell] value)
      :unset-active-cell (assoc-in state [:table-editor :active-cell] nil)

      :move-active-cell
      (assoc-in state
                [:table-editor :active-cell]
                (calculate-move-cell state value))
      :set-fragments
      (assoc-in state
                [:table-editor :fragments]
                (->> value (sort-by :offset) vec))

      :add-row
      (let [fragment-index (-> state :table-editor :fragments count dec)]
        (if (=
             (dec (get-in state (meta-path :count)))
             (get-in state [:table-editor :fragments fragment-index :last-row]))
          state
          (-> state
              (update-in [:table-editor :fragments fragment-index :data] conj {})
              (update-in [:table-editor :fragments fragment-index :last-row] inc)
              (update-in (meta-path :count) inc)
              update-data-preview)))
      
      :init-closing (assoc-in state [:table-editor :closing] true)
      :set-mode (assoc-in state [:table-editor :mode] value)
      :set-view-rows (assoc-in state [:table-editor :view-rows] value)
      :set-loading-view (assoc-in state [:table-editor :loading-view] value)
      state)))

;; -------------------------
;; Task

(defn fetch-fragments [row-index]
  (emit [:set-loading-view true] handler)
  (let [result (atom [])
        limit (->> (get-in @editor-cursor [:meta :sort-columns])
                   (count)
                   (max 1)
                   (/ 1000)
                   (js/Math.floor))
        first-row (-> row-index
                      (- 5)
                      (max 0))
        read-callback (fn [data]
                        (if (and (some? data)
                                 (< (:first-row data) (+ first-row limit)))
                          (do (swap! result conj data) true)
                          (do 
                            (emit [:set-fragments @result] handler)
                            (emit [:set-loading-view false] handler)
                            false)))]
    (emit [:set-view-rows [first-row (or row-index 0) limit]] handler)
    (data/read-row-fragments {:object-id (get-in @editor-cursor [:meta :id])
                              :start-row first-row
                              :cursor-callback read-callback})))

(defn init-fragment [table-meta]
  (let [new-fragment (init-fragment-data (get-in @editor-cursor [:meta :id]))
        success-callback (fn []
                           (emit [:set-fragments [new-fragment]] handler)
                           (emit [:set-view-rows [0 0 100]] handler))
        error-callback #(js/console.error "Failed to create fragment" %)]
    (data/put-fragments {:data [new-fragment]
                         :on-success success-callback
                         :on-error error-callback})))

(defn init-table []
  (let [new-table (init-table-data)
        success-callback (fn []
                           (emit [:set-editor-table new-table])
                           (init-fragment new-table))
        error-callback #(js/console.error "Failed to create table!" %)]
    (data/put-meta {:data new-table
                    :on-success success-callback
                    :on-error error-callback})))

(defn load-existing-table [table]
  (emit [:open-editor-table table])
  (fetch-fragments 0))

(defn load-table-editor [table]
  (if (= table :new)
    (init-table)
    (load-existing-table table)))

(def store-meta-queue (r/atom 0))

(defn store-meta []
  (reset! store-meta-queue 0)
  (let [data (:meta @editor-cursor)
        success-callback #(emit [:set-table data])
        error-callback #(js/console.error "Failed to create table!" %)]
    (data/put-meta {:data data
               :on-success success-callback
               :on-error error-callback})))

(defn debounce-store-meta []
  (go
    (swap! store-meta-queue inc)
    (<! (timeout debounce-timeout))
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
    (data/put-fragments {:data (:fragments @editor-cursor)
                         :on-success success-callback
                         :on-error error-callback})))

(defn debounce-store-fragment []
  (go
    (swap! store-fragment-queue inc)
    (<! (timeout debounce-timeout))
    (case @store-fragment-queue
      0 nil ; Queue was shortcut
      1 (store-fragment)
      (swap! store-fragment-queue dec))))

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
    (data/delete-table {:table-id (:id table)
                        :on-complete delete-callback})
    (close-modal)))

;; -------------------------
;; View

(defn editor-name []
  [:div
   {:class "modal-section name-editor"}
   [:h3 "Name"]
   [:div
    {:class "input-wrapper"}
    [:input
     {:class "input"
      :type "text"
      :value (get-in @editor-cursor [:meta :name])
      :auto-focus true
      :placeholder "New table name"
      :on-change #(emit-edit-meta [:set-table-name (.. % -target -value)])}]]])

(defn editor-column [column-id]
  [:div
   {:key column-id :class "column-editor"}
   [:div
    {:class "input-wrapper column-label"}
    [:input
     {:class "input"
      :type "text"
      :value (get-in @editor-cursor [:meta :columns column-id :name])
      :auto-focus true
      :placeholder "New column name"
      :on-change #(emit-edit-meta
                    [:set-column-name [column-id (.. % -target -value)]])}]]
   [:div
    {:class "input-wrapper column-type"}
    [:select
     {:class "select"
      :value (get-in @editor-cursor [:meta :columns column-id :type])
      :on-change #(emit-edit-meta
                    [:set-column-type [column-id (.. % -target -value)]])}
     [:option {:value :string} "Text"]
     [:option {:value :boolean} "Boolean"]
     [:option {:value :number} "Number"]]]])

(defn editor-columns []
  [:div {:class "modal-section column-set-editor"}
   [:h3 "Columns"]
   (doall (for [column-id
                (get-in @editor-cursor [:meta :sort-columns])]
            (editor-column column-id)))
   [:button
    {:class "btn add-column-btn"
     :on-click #(emit-edit-meta [:add-column nil])}
    "Add column"]])

(defn cell-editor [column-type]
  (let [[fragment-index row-index column-id] (:active-cell @editor-cursor)
        value (get-in
                @editor-cursor
                [:fragments fragment-index :data row-index column-id])
        on-change (fn [event]
                    (as-> (.. event -target -value) v
                      (if (= :number (keyword column-type))
                        (re-find #"[0-9]*\.?[0-9]*" v)
                        v)
                      (emit-edit-fragment [:edit-cell v])))]
    [:input
     {:class "input"
      :type (if (= column-type :number) "number" "text")
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

(defn editor-cell [{:keys [row-index column-id data]}]
  (let [fragment-index (->> (:fragments @editor-cursor)
                            (map :first-row)
                            (filter #(>= row-index %))
                            first)
        cell-key [fragment-index row-index column-id]
        activate-cell #(emit [:set-active-cell cell-key] handler)]
    (cond
      (= cell-key (:active-cell @editor-cursor))
      [:td.active
       (cell-editor
         (get-in @editor-cursor [:meta :columns column-id :type]))]

      (blank? data)
      [:td {:class "blank" :on-click activate-cell} "Blank"]

      :else
      [:td {:on-click activate-cell} (str data)])))

(defn settings []
  [:div
   {:class "modal-body modal-editor"}
   [:div
    {:class "table-editor"}
    [editor-name]
    [editor-columns]]])

(defn get-rows [fragments start-row limit]
  (reduce
    (fn [result-rows fragment]
      (if (>= (:first-row fragment) start-row)
        (concat result-rows
                (take (- limit (count result-rows)) (:data fragment)))
        (subvec (:data fragment)
                (- start-row (:first-row fragment))
                (-> (+ start-row limit)
                    (min (:last-row fragment))
                    (- (:first-row fragment))))))
    []
    (->> fragments
         (filter #(< start-row (:last-row %)))
         (filter #(> (+ start-row limit) (:first-row %))))))

(defn data-view-selector [{:keys [value]}]
  [:div
   {:class "input-wrapper select-row"}
   [:input
    {:class "input"
     :type "number"
     :value value
     :auto-focus true
     :placeholder "Get row"
     :max (-> @editor-cursor :meta :count dec)
     :on-change #(fetch-fragments (.. % -target -value))}]])

(defn data-view-table [{:keys [start-row limit]}]
  [table-component
   {:sort-columns (get-in @editor-cursor [:meta :sort-columns])
    :columns (get-in @editor-cursor [:meta :columns])
    :data-rows (get-rows (:fragments @editor-cursor) start-row limit)
    :row-offset start-row
    :cell-component editor-cell}])

(defn data-view []
  (let [[start-row target limit] (:view-rows @editor-cursor)]
    [:div
     {:class "modal-body modal-data"}
     [:h3 "Data"]
     [data-view-selector {:value (or target 0)}]
     (if (:loading-view @editor-cursor)
       [:div.loading "Loading data..."]
       [data-view-table {:start-row start-row :limit limit}])
     (when (>= limit (-> @editor-cursor :meta :count dec))
      [:button
       {:class "btn add-row-btn"
        :on-click #(emit [:add-row nil] handler)}
       "Add row"])]))

(defn table-editor []
  [:div
   {:class (if (:closing @editor-cursor)
             "modal modal-closing"
             "modal ")}
   [:div
    {:class "modal-header"}
    [:div {:class "modal-title"} "Table"]
    [:div {:class "modal-menu btn-group"}
     [:button
      {:class "btn setup-btn" :on-click #(emit [:set-mode :setup] handler)}
      "Setup"]
     [:button
      {:class "btn data-btn" :on-click #(emit [:set-mode :data] handler)}
      "Data"]
     [:button {:class "btn btn-delete" :on-click delete} "Delete"]
     [:button {:class "btn btn-close" :on-click close-table-editor} "Close"]]]
   (if (= :data (:mode @editor-cursor))
     [data-view]
     [settings])])
