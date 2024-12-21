(ns bord.editor
  (:require
    [reagent.core :as r]
    [reagent.dom :as d]
    [clojure.string :refer [blank?]]
    ["react" :as react]))

(defn table-editor-default-column [index]
   {:tableId :new
    :index index
    :type "string"})

(defn table-editor-default-state []
  {:new true
   :name ""
   :columns [(table-editor-default-column 0)]
   :rows [{}]
   :active-cell nil
   :count 0})

(defonce table-editor-state (r/atom nil))

(defn table-editor-init-state
  ([] (reset! table-editor-state (table-editor-default-state)))
  ([data] (reset! table-editor-state {:new false
                                      :tableId (.-tableId data)
                                      :name (.-name data)
                                      :columns []
                                      :rows []
                                      :active-cell nil
                                      :edit-cell ""
                                      :count 0})))

(defn apply-edit-cell [state]
  (if (some? (:active-cell state))
    (let [[active-row active-column] (:active-cell state)
          updated-rows (assoc-in (:rows state) (:active-cell state) (:edit-cell state))]
      (assoc state :rows updated-rows :edit-cell ""))
    state))

(defn add-column [msg state]
  (let [new-column (-> @state :columns count table-editor-default-column)]
    (swap! state update-in [:columns] #(conj % new-column))))

(defn add-row [msg state]
  (swap! state update-in [:rows] #(conj % (or msg {}))))

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
  (let [[row column] (:active-cell @state)
        last-row (-> @state :rows count dec)
        last-column (-> @state :columns count dec)]
    (case msg
      :down (if (< row last-row)
                (set-active-cell [(inc row) column] state))
      :prev (if (> column 0)
              (set-active-cell [row (dec column)] state)
              (set-active-cell [(dec row) last-column] state))
      :next (if (< column last-column)
              (set-active-cell [row (inc column)] state)
              (set-active-cell [(inc row) 0] state))
      nil)))

(defn set-column-name [msg state]
  (let [[column-key value] msg]
    (swap! state assoc-in [:columns column-key :name] value)))

(defn set-column-type [msg state]
  (let [[column-key value] msg]
    (swap! state assoc-in [:columns column-key :type] value)))

(defn table-data-payload [state]
  (-> state
    (select-keys [:tableId :name :updated :columns :count])
    (assoc :created (if (:new state) (js/Date.now) (:created state))
           :updated (js/Date.now))))

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

(defn table-editor-column-set []
  [:div {:class "editor-section column-set-editor"}
   [:h3 "Columns"]
   (doall (for [column-index (-> @table-editor-state :columns count range)]
            (table-editor-column column-index)))
   [:button {:class "btn add-column-btn"
             :on-click #(add-column nil table-editor-state)}
    "Add column"]])

(defn table-editor-column [column-key]
  [:div {:key column-key :class "column-editor"}
   [:div {:class "input-wr"}
    [:input
     {:type "text"
      :value (-> @table-editor-state :columns (nth column-key) :name)
      :auto-focus true
      :placeholder "New column name"
      :on-change #(set-column-name [column-key (.. % -target -value)] table-editor-state)}]]
   [:div {:class "input-wr"}
    [:select
     {:value (-> @table-editor-state :columns (nth column-key) :type)
      :on-change #(set-column-type [column-key (.. % -target -value)] table-editor-state)}
     [:option { :value :string } "Text"]
     [:option { :value :number } "Number"]]]])


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

(defn table-editor-cell [column row-key data]
  (let [cell-key [row-key (:index column)]
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
        (for [{name :name index :index} (:columns @table-editor-state)]
          (if (blank? name)
            [:th {:key index :class "blank"} "Blank"]
            [:th {:key index} name])))]
     (doall
       (for [[row-key row-data] (map-indexed vector (:rows @table-editor-state))]
         [:tr {:key row-key}
          (doall
            (for [column (:columns @table-editor-state)]
              (table-editor-cell column row-key (get row-data (:index column)))))]))]]
   [:button {:class "btn add-row-btn"
             :on-click #(add-row nil table-editor-state)}
    "Add row"]])


(defn table-editor [{:keys [on-save on-close]}]
  (js/console.log (str @table-editor-state))
  (let [new-table (:new @table-editor-state)
        save-text (if new-table "Create" "Update")]
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
