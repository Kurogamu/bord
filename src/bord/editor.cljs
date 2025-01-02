(ns bord.editor
  (:require
    [reagent.core :as r]
    [reagent.dom :as d]
    [clojure.string :refer [blank?]]
    ["react" :as react]))

(defn table-editor-default-column []
   {:action :create
    :id (js/crypto.randomUUID)
    :type "string"})

(defn table-editor-default-state []
  (let [column (table-editor-default-column)]
    {:action :create
     :id (js/crypto.randomUUID)
     :name ""
     :columns {(:id column) column}
     :sort-columns [(:id column)]
     :rows [{}]
     :rows-changed true
     :active-cell nil
     :count 0}))

(defonce table-editor-state (r/atom nil))

(defn table-editor-init-state
  ([] (reset! table-editor-state (table-editor-default-state)))
  ([data]
   (js/console.log data)
   (reset! table-editor-state 
           (-> data
               (select-keys [:id :name :sort-columns :columns :rows])
               (assoc :action :update
                      :active-cell nil
                      :edit-cell "")))))

(defn apply-edit-cell [state]
  (if (some? (:active-cell state))
    (let [[active-row active-column] (:active-cell state)
          updated-rows (assoc-in (:rows state) (:active-cell state) (:edit-cell state))]
      (assoc state :rows updated-rows :edit-cell "" :rows-changed true))
    state))

(defn add-column [msg state]
  (let [new-column (table-editor-default-column)]
    (swap! state
           (fn [prev-state]
             (-> prev-state
                 (update :sort-columns #(conj % (:id new-column)))
                 (assoc-in [:columns (:id new-column)] new-column))))))

(defn add-row [msg state]
  (swap! state
         (fn [prev-state]
           (-> prev-state
               (assoc :rows-changed true)
               (update-in [:rows] #(conj % (or msg {})))))))

(defn unset-active-cell [msg state]
  (swap! state #(-> %
                    apply-edit-cell
                    (assoc :active-cell nil :edit-cell ""))))

(defn set-active-cell [msg state]
  (let [next-value (or (get-in (:rows @state) msg) "")]
    (swap! state #(-> %
                      apply-edit-cell
                      (assoc :active-cell msg :edit-cell next-value)))))

(defn move-active-cell [msg state]
  (let [[row column-id] (:active-cell @state)
        column-index (.indexOf (:sort-columns @state) column-id)
        last-row (-> @state :rows count dec)
        last-column (-> @state :columns count dec)]
    (case msg
      :down (if (< row last-row)
                (set-active-cell [(inc row) (nth (:sort-columns @state) column-index)] state))
      :prev (if (> column-index 0)
              (set-active-cell [row (nth (:sort-columns @state) (dec column-index))] state)
              (set-active-cell [(dec row) (last (:sort-columns @state))] state))
      :next (if (< column-index last-column)
              (set-active-cell [row (nth (:sort-columns @state) (inc column-index))] state)
              (set-active-cell [(inc row) (first (:sort-columns @state))] state))
      nil)))

(defn set-column-name [msg state]
  (let [[column-key value] msg]
    (swap! state assoc-in [:columns column-key :name] value)))

(defn set-column-type [msg state]
  (let [[column-key value] msg]
    (swap! state assoc-in [:columns column-key :type] value)))

(defn table-meta-payload [state]
  (-> state
    (select-keys [:action :name :updated :count :sort-columns])
    (assoc 
      :tableId (:id state)
      :created (if (= (:action state) :create) (js/Date.now) (:created state))
      :updated (js/Date.now))))

(defn columns-payload [state]
  (map (fn [column]
         {:tableId (:id state)
          :columnId (:id column)
          :action (:action column)
          :type (:type column)
          :name (:name column)})
       (vals (:columns state))))

(defn rows-payload [state] ; SOLVE THIS
  (if (:rows-changed state)
    {:fragmentId (js/crypto.randomUUID)
     :tableId (:id state)
     :first-row 0
     :last-row (count (:rows state))
     :data (:rows state)}))

(defn table-data-payload [state]
  {:meta (table-meta-payload state)
   :rows (rows-payload state)
   :columns (columns-payload state)})

(defn table-editor-name []
  [:div {:class "editor-section name-editor"}
   [:h3 "Name"]
   [:div {:class "input-wr"}
    [:input
     {:type "text"
      :value (:name @table-editor-state)
      :auto-focus true
      :placeholder "New table name"
      :on-change #(swap! table-editor-state assoc :name (.. % -target -value))}]]])

(defn table-editor-column [column-id]
  [:div {:key column-id :class "column-editor"}
   [:div {:class "input-wr"}
    [:input
     {:type "text"
      :value (get-in @table-editor-state [:columns column-id :name])
      :auto-focus true
      :placeholder "New column name"
      :on-change #(set-column-name [column-id (.. % -target -value)] table-editor-state)}]]
   [:div {:class "input-wr"}
    [:select
     {:value (get-in @table-editor-state [:columns column-id :type])
      :on-change #(set-column-type [column-id (.. % -target -value)] table-editor-state)}
     [:option { :value :string } "Text"]
     [:option { :value :number } "Number"]]]])

(defn table-editor-column-set []
  [:div {:class "editor-section column-set-editor"}
   [:h3 "Columns"]
   (doall (for [column-id (:sort-columns @table-editor-state)]
            (table-editor-column column-id)))
   [:button {:class "btn add-column-btn"
             :on-click #(add-column nil table-editor-state)}
    "Add column"]])


(defn table-cell-editor []
  [:input
   {:type "text"
    :auto-focus true
    :value (:edit-cell @table-editor-state)
    :placeholder "New value"
    :on-change #(swap! table-editor-state assoc :edit-cell (.. % -target -value))
    :on-blur #(unset-active-cell nil table-editor-state)
    :on-key-down (fn [e]
                   (case (.-key e)
                     "Enter" (do
                               (.preventDefault e)
                               (move-active-cell :down table-editor-state))
                     "Tab" (do
                             (.preventDefault e)
                             (if (.-shiftKey e)
                               (move-active-cell :prev table-editor-state)
                               (move-active-cell :next table-editor-state)))
                     nil))}])

(defn table-editor-cell [column-id row-key data]
  (let [cell-key [row-key column-id]
        activate-cell #(set-active-cell cell-key table-editor-state)]
    (cond
      (= cell-key (:active-cell @table-editor-state))
        [:td {:key cell-key :class "active"} (table-cell-editor)]
      (blank? data)
        [:td {:key cell-key :class "blank" :on-click activate-cell} "Blank"]
      :else
        [:td {:key cell-key :on-click activate-cell} (str data)])))

(defn table-editor-data []
  [:div {:class "editor-section table-editor"}
   [:h3 "Data"]
   [:div {:class "table-wr"}
    [:table
     [:tr
      (doall
        (for [column-id (:sort-columns @table-editor-state)]
          (let [name (get-in @table-editor-state [:columns column-id :name])]
            (if (blank? name)
              [:th {:key column-id :class "blank"} "Blank"]
              [:th {:key column-id} name]))))]
     (doall
       (for [[row-key row-data] (map-indexed vector (:rows @table-editor-state))]
         [:tr {:key row-key}
          (doall
            (for [column-id (:sort-columns @table-editor-state)]
              (table-editor-cell column-id row-key (get row-data column-id))))]))]]
   [:button {:class "btn add-row-btn"
             :on-click #(add-row nil table-editor-state)}
    "Add row"]])


(defn table-editor [{:keys [on-save on-close]}]
  (js/console.log (str @table-editor-state))
  (let [is-new-table (= (:create (:action @table-editor-state)))
        save-text (if is-new-table "Create" "Update")]
    [:div {:class "modal"}
     [:div {:class "modal-header"} "Table Editor"]
     [:div {:class "modal-body"}
      [:div {:class "table-editor"}
       [table-editor-name]
       [table-editor-column-set]
       [table-editor-data ]]]
     [:div {:class "modal-footer"}
      [:button {:class "cancel"
                :on-click on-close}
       "Cancel"]
      [:button {:class "save"
                :on-click #(on-save (table-data-payload @table-editor-state))}
       save-text]]]))
