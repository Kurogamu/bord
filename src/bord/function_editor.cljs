(ns bord.function-editor
  (:require
    [reagent.core :as r]
    [clojure.string :refer [blank?]]
    [bord.state :refer [app-state emit function-outputs]]
    [bord.common :refer [find-first-i remove-i move-i remove-match swap-i]]
    [bord.data :refer [put-function delete-function]]
    [bord.function :refer [all-operations
                           function-types
                           param-type result-type process-row]]
    [bord.select-modal :refer [multiselect]]
    [cljs.core.async :refer [go timeout]]
    ["react" :as react]))

;; -------------------------
;; Model

(def editor-cursor (r/cursor app-state [:function-editor :function]))

(defn source-columns [state]
  (let [source-id (get-in state [:function-editor :function :source])]
    (get-in state [:tables source-id :columns])))

(defn source-number-columns [state]
  (->> (source-columns state)
       (filter #(= :number (keyword (:type (second %)))))
       (into {})))

(defn get-preview-data [state]
  (let [source-id (get-in state [:function-editor :function :source])]
    (get-in state [:tables source-id :data-preview])))

(defn init-function-data [state]
  {:id (js/crypto.randomUUID)
   :name ""
   :created (js/Date.now)
   :updated (js/Date.now)
   :source (-> state :tables first first)
   :outputs []
   :type :map
   :sort-operations []
   :operations {}})

(defn init-operation [state]
  (let [default-label
        (-> (get-in state [:function-editor :function :sort-operations])
            count
            (str "r"))]
    {:id (js/crypto.randomUUID)
     :operand :number-constant
     :name default-label
     :params nil}))

;; -------------------------
;; Update

(defn handler [state [event value]]
  (let [data-path (fn [& args] (concat [:function-editor :function] args))]
    (case event
      :set-name (assoc-in state (data-path :name) value)
      :set-source (assoc-in state (data-path :source) value)

      :set-type
      (-> state
          (assoc-in (data-path :operations) {})
          (assoc-in (data-path :sort-operations) [])
          (assoc-in (data-path :outputs) [])
          (assoc-in (data-path :type) value))

      :add-operation
      (-> state
          (assoc-in (data-path :operations (:id value)) value)
          (update-in (data-path :sort-operations) conj (:id value))
          (assoc-in [:function-editor :active-operation] (:id value)))

      :move-back-operation
      (let [index
            (find-first-i (get-in state (data-path :sort-operations)) value)]
        (update-in state
                   (data-path :sort-operations)
                   swap-i index (inc index)))

      :move-forward-operation
      (let [index (find-first-i
                    (get-in state (data-path :sort-operations))
                    value)]
        (update-in state
                   (data-path :sort-operations)
                   swap-i index (dec index)))

      :delete-operation
      (-> state
          (update-in (data-path :operations) dissoc value)
          (update-in (data-path :sort-operations) remove-match value))

      :set-operand
      (let [[operator operand] value]
        (update-in state (data-path :operations operator) dissoc :params)
        (assoc-in state (data-path :operations operator :operand) operand))

      :set-params
      (let [[operator params] value]
        (assoc-in state (data-path :operations operator :params) params))

      :set-label
      (let [[operator label] value]
        (assoc-in state (data-path :operations operator :name) label))
      :set-outputs (assoc-in state (data-path :outputs) value)
      :set-preview (assoc-in state (data-path :preview) value)
      :init-closing (assoc-in state [:function-editor :closing] true)

      :set-active-operation (assoc-in state [:function-editor :active-operation] value)
      :set-reorder-operations (assoc-in state [:function-editor :reorder-operations] value)
      :set-moving-operation (assoc-in state [:function-editor :moving-operation] value)
      :set-dragover-operation (assoc-in state [:function-editor :dragover-operation] value)
      :set-moving-operation-destination
      (let [source (find-first-i 
                     (get-in state (data-path :sort-operations))
                     (get-in state [:function-editor :moving-operation]))
            target (find-first-i 
                     (get-in state (data-path :sort-operations))
                     value)]
        (-> state
            (assoc-in [:function-editor :dragover-operation] nil)
            (assoc-in [:function-editor :moving-operation] nil)
            (update-in (data-path :sort-operations) move-i source target)))

      state)))

;; -------------------------
;; Task

(defn setup-new-function []
  (let [new-function (init-function-data @app-state)
        success-callback #(emit [:set-editor-function new-function])
        error-callback #(js/console.error "Failed to create function" %)]
    (put-function
      {:data new-function
       :on-complete success-callback
       :on-error error-callback})))

(defn load-function-editor [function]
  (if (= function :new)
    (setup-new-function)
    (emit [:set-editor-function function])))

(def store-function-queue (r/atom 0))

(defn store-function []
  (reset! store-function-queue 0)
  (let [data @editor-cursor
        success-callback #(js/console.info "Data saved")
        error-callback #(js/console.error "Failed to store data: " %)]
    (put-function
      {:data data
       :on-complete success-callback
       :on-error error-callback})))

(defn debounce-store-function []
  (go
    (swap! store-function-queue inc)
    (<! (timeout 2000))
    (case @store-function-queue
      0 nil ; Queue was shortcut
      1 (store-function)
      (swap! store-function-queue dec))))

(defn calculate-preview []
  (let [results (mapv
                  #(process-row % @editor-cursor)
                  (get-preview-data @app-state))]
    (emit [:set-preview results] handler)))

(defn emit-edit [msg]
  (emit msg handler)
  (calculate-preview)
  (debounce-store-function))

(defn close-modal []
  (js/window.addEventListener
    "animationend"
    #(if (= (.-animationName %) "slide-out") (emit [:close-editor nil]))
    #js {:once true})
  (emit [:init-closing nil] handler))

(defn close-editor []
  (store-function)
  (emit [:set-function @editor-cursor])
  (close-modal))

(defn delete []
  (let [data @editor-cursor
        delete-callback #(emit [:delete-function data])]
    (delete-function {:function-id (:id data)
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
    [:input {:type "text"
             :value (:name @editor-cursor)
             :auto-focus true
             :placeholder "New function name"
             :on-change #(emit-edit [:set-name (.. % -target -value)])}]]])

(defn editor-data-source []
  [:div
   {:class "modal-section data-source-editor"}
   [:h3 "Data source"]
   [:div
    {:class "input-wrapper"}
    [:select
     {:value (:source @editor-cursor)
      :on-change #(emit-edit [:set-source (.. % -target -value)])}
     (for [table (vals (:tables @app-state))]
       [:option
        {:key (:id table) :value (:id table)}
        (:name table)])]]])

(defn editor-type []
  [:div
   {:class "modal-section type-editor"}
   [:h3 "Function type"]
   [:div
    {:class "input-wrapper"}
    [:select
     {:value (:type @editor-cursor)
      :on-change #(emit-edit [:set-type (.. % -target -value)])}
     (for [[function-key function-type] function-types]
       [:option
        {:key function-key :value function-key}
        (:label function-type)])]]])

(defn editor-parameters-select [{:keys [id value value-type]}]
  (let [preceding-operations
        (->>
          (find-first-i (:sort-operations @editor-cursor) id)
          (subvec (:sort-operations @editor-cursor) 0)
          (map #(vector % (get-in @editor-cursor [:operations %]))))

        selectable-operations
        (if (= value-type :source-number)
          (->> preceding-operations
               (filter #(= :number (-> % second :operand keyword result-type)))
               (into {}))
          (into {} preceding-operations))

        selectable-columns
        (if (= value-type :source-number)
          (source-number-columns @app-state)
          (source-columns @app-state))

        selectable (merge selectable-operations selectable-columns)]
    [:<>
     [:div {:class "field-label"} "Parameters"]
     [:div {:class "field-wrapper"}
      [:div {:class "button-select-wrapper"}
       (if (some? selectable)
         [:div
          {:class "select-group-wrapper"}
          [:div
           {:class "btn-group select-group"}
           (for [[option-key option] selectable]
             [:button
              {:class "btn btn-select"
               :key option-key
               :on-click #(emit-edit [:set-params [id (conj value option-key)]])}
              (:name option)])]
          [:div {:class "label"} "Available"]]
         [:div {:class "blank"} "No parameters available"])
       (if (some? value)
         [:div
          {:class "select-group-wrapper"}
          [:div
           {:class "btn-group select-group"}
           (for [[index param-key] (map-indexed vector value)]
             [:button
              {:class "btn btn-select"
               :key index
               :on-click #(emit-edit [:set-params [id (remove-i value index)]])}
              (:name (get selectable param-key))])]
          [:div {:class "label"} "Selected"]]
         [:div {:class "blank"} "No parameters selected"])]]]))

(defn editor-parameters-value [{:keys [id value value-type]}]
  (let [input-type (if (= param-type :input-number) "number" "text")]
    [:<>
     [:div {:class "input-label"} "Value"]
     [:input
      {:class "input"
       :type input-type
       :value value
       :on-change #(emit-edit [:set-params [id (.. % -target -value)]])}]]))

(defn editor-parameters [{:keys [id operand params]}]
  (let [value-type (param-type (keyword operand))
        props {:id id :value-type value-type :value params}]
    (case value-type
      :source-number [editor-parameters-select props]
      :source-string [editor-parameters-select props]
      [editor-parameters-value props])))

(defn editor-operation-edit [operation]
  [:div
   {:key (:id operation)
    :class "operation operation-edit grid-editor"}
   [:div {:class "input-label"} "Name"]
   [:input
    {:class "input-wrapper"
     :type "text"
     :value (:name operation)
     :placeholder "Name"
     :on-change
     #(emit-edit [:set-label [(:id operation) (.. % -target -value)]])}]
   [:div {:class "input-label"} "Function"]
   [:select
    {:class "input-wrapper"
     :value (:operand operation)
     :on-change
     #(emit-edit [:set-operand [(:id operation) (.. % -target -value)]])}
    (for [[op-key op] (get-in function-types [(keyword (:type @editor-cursor)) :operations])]
      [:option {:key op-key :value op-key} (:label op)])]
  (editor-parameters operation)
  [:div
   {:class "button-wrapper"}
   [:button
    {:class "btn"
     :on-click #(emit [:set-active-operation nil] handler)}
    "Done"]]])

(defn editor-operation-view [operation]
  (let [operand (get all-operations (keyword (:operand operation)))]
    [:div
     {:key (:id operation)
      :class "operation operation-view"
      :on-click #(emit [:set-active-operation (:id operation)] handler)}
     [:div
      {:class "operation-view-header"}
      [:div {:class "operation-view-name"} (:name operation)]
      [:div {:class "operation-view-operand"} (:label operand)]]
     [:div
      {:class "operation-view-params"}
      (case (:param-type operand)
        :input-number [:div {:class "number"} (:params operation)]
        :input-string [:div {:class "string"} (:params operation)]
        (let [sources (merge (source-columns @app-state)
                             (:operations @editor-cursor))
              param-labels (map #(-> (get sources %) :name) (:params operation))]
          [:div
           {:class "list"}
           (map-indexed (fn [i l] [:span {:key i} l]) param-labels)]))]]))

(defn editor-operation [id]
  (let [operation (get-in @editor-cursor [:operations id])
        active (= (get-in @app-state [:function-editor :active-operation]) id)]
    (if active
      (editor-operation-edit operation)
      (editor-operation-view operation))))

(defn editor-operation-movable [operation]
  (let [moving-operation (get-in @app-state [:function-editor :moving-operation])
        dragover-operation (get-in @app-state [:function-editor :dragover-operation])
        class-name (condp = (:id operation)
                     moving-operation
                     "operation-movable operation-movable-moving"
                     dragover-operation
                     "operation-movable operation-movable-dragover"
                     "operation-movable")
        on-drag (fn [e]
                  (.preventDefault e)
                  (if (not= moving-operation (:id operation))
                    (emit [:set-moving-operation (:id operation)] handler)))
        on-drag-over (fn [e]
                      (.preventDefault e)
                      (if (not= dragover-operation (:id operation))
                        (emit [:set-dragover-operation (:id operation)] handler)))]
    [:div
     {:key (:id operation)
      :class class-name
      :draggable true
      :droppable true
      :on-drag on-drag
      :on-drag-end #(emit [:set-moving-operation nil] handler)
      :on-drag-over on-drag-over
      :on-drag-leave #(emit [:set-dragover-operation nil] handler)
      :on-drop (fn [e] (emit-edit [:set-moving-operation-destination (:id operation)]))}
     [:span (:name operation)]]))

(defn editor-operations-reorder []
  (let [operations (map
                     #(get-in @editor-cursor [:operations %])
                     (:sort-operations @editor-cursor))]
    [:<>
    [:div
     {:class "movable-operation-wrapper"}
     (doall
       (for [operation operations]
         (editor-operation-movable operation)))]
    [:div {:class "btn-group"}
     [:button
      {:class "btn reorder-btn"
       :on-click #(emit [:set-reorder-operations false] handler)}
      "Done"]]]))

(defn editor-operations []
  [:div
   {:class "modal-section output-operations"}
   [:h3 "Operations"]
   (if (get-in @app-state [:function-editor :reorder-operations])
     [editor-operations-reorder]
     [:<>
      (doall (map editor-operation (:sort-operations @editor-cursor)))
      [:div {:class "btn-group"}
       [:button
        {:class "btn add-operation-btn"
         :on-click #(emit-edit [:add-operation (init-operation @app-state)])}
        "Add"]
       [:button
        {:class "btn reorder-btn"
         :on-click #(emit [:set-reorder-operations true] handler)}
        "Reorder"]]])])

(defn editor-output []
  (let [options (merge
                  (source-columns @app-state)
                  (:operations @editor-cursor))]
    [:div
     {:class "modal-section output-editor-container"}
     [:h3 "Outputs"]
     [multiselect
      {:on-change #(emit-edit [:set-outputs %])
       :value (:outputs @editor-cursor)
       :options options
       :labelfn :name
       :header "Outputs"}]]))

(defn editor-preview-table []
  [:table
   [:tr
    (doall
      (for [output (function-outputs @editor-cursor)]
        [:th {:key (:id output)} (:name output)]))]
   (for [[index result-row]
         (map-indexed vector (:preview @editor-cursor))]
     [:tr {:key index}
      (for [[id value] result-row]
        [:td {:key id} (or (str value) "Blank")])])])

(defn editor-preview []
  [:div
   {:class "modal-section preview"}
   [:h3 "Preview"]
   (if (not-empty (:outputs @editor-cursor))
     [:div {:class "table-wrapper"} (editor-preview-table)]
     [:div "No data available"])])

(defn function-editor []
  [:div
   {:class (if (:closing (:function-editor @app-state))
             "modal modal-editor modal-editor-closing"
             "modal modal-editor")}
   [:div
    {:class "modal-header"}
    [:div {:class "modal-title"} "Function Editor"]
    [:div
     {:class "modal-menu btn-group"}
     [:button {:class "btn delete-btn" :on-click delete} "Delete"]
     [:button {:class "btn close-btn" :on-click close-editor} "Close"]]]
   [:div
    {:class "modal-body"}
    [editor-name]
    [editor-data-source]
    [editor-type]
    [editor-operations]
    [editor-output]
    [editor-preview]]])
