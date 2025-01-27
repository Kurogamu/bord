(ns bord.function-editor
  (:require
    [reagent.core :as r]
    [clojure.string :refer [blank?]]
    [bord.state :refer [app-state emit]]
    [bord.common :refer [find-first-i remove-match swap-entry]]
    [bord.data :refer [put-function delete-function]]
    [bord.function :refer [function-types all-operations process-row]]
    [bord.select-modal :refer [multiselect]]
    [cljs.core.async :refer [go timeout]]
    ["react" :as react]))

;; -------------------------
;; Model

(def editor-cursor (r/cursor app-state [:function-editor]))

(defn get-source-columns [state]
  (let [source-id (get-in state [:function-editor :data :source])]
    (->> (:tables state)
        (filter #(= source-id (:id %)))
        first
        :columns)))

(defn get-source-number-columns [state]
  (into {}
        (filter
          #(= :number (keyword (:type (second %))))
          (get-source-columns state))))

(defn get-preview-data [state]
  (let [source-id (get-in state [:function-editor :data :source])]
    (->> (:tables state)
        (filter #(= source-id (:id %)))
        first
        :data-preview)))

(defn init-function-data []
  {:id (js/crypto.randomUUID)
   :name ""
   :created (js/Date.now)
   :updated (js/Date.now)
   :outputs []
   :type :map
   :sort-operations []
   :operations {}})

(defn init-operation [name]
  {:id (js/crypto.randomUUID)
   :operand :number-constant
   :name (or name "")
   :params [] })

;; -------------------------
;; Update

(defn handler [state [event value]]
  (case event
    :set-name (assoc-in state [:function-editor :data :name] value)
    :set-source (assoc-in state [:function-editor :data :source] value)

    :set-type
    (-> state
        (assoc-in [:function-editor :data :operations] {})
        (assoc-in [:function-editor :data :sort-operations] [])
        (assoc-in [:function-editor :data :outputs] [])
        (assoc-in [:function-editor :data :type] value))

    :add-operation 
    (-> state
        (assoc-in [:function-editor :data :operations (:id value)] value)
        (update-in [:function-editor :data :sort-operations] conj (:id value)))

    :move-back-operation 
    (let [index (find-first-i
                  (get-in state [:function-editor :data :sort-operations])
                  value)]
      (update-in state
                 [:function-editor :data :sort-operations]
                 swap-entry index (inc index)))

    :move-forward-operation 
    (let [index (find-first-i
                  (get-in state [:function-editor :data :sort-operations])
                  value)]
      (update-in state
                 [:function-editor :data :sort-operations]
                 swap-entry index (dec index)))

    :delete-operation 
    (-> state
        (update-in [:function-editor :data :operations] dissoc value)
        (update-in [:function-editor :data :sort-operations] remove-match value))

    :set-operand
    (let [[operator operand] value]
      (assoc-in state [:function-editor :data :operations operator :params] [])
      (assoc-in state [:function-editor :data :operations operator :operand] operand))

    :set-params
    (let [[operator params] value]
      (assoc-in state [:function-editor :data :operations operator :params] params))

    :set-label
    (let [[operator label] value]
      (assoc-in state [:function-editor :data :operations operator :name] label))

    :set-outputs
    (assoc-in state [:function-editor :data :outputs] value)

    state))

;; -------------------------
;; Task

(defn setup-new-function []
  (let [new-function (init-function-data)
        success-callback #(emit [:set-editor-function new-function])
        error-callback #(js/console.error "Failed to create function" %)]
    (put-function {:data new-function
                   :on-complete success-callback
                   :on-error error-callback})))

(defn load-function-editor [function]
  (if (= function :new)
    (setup-new-function)
    (emit [:set-editor-function function])))

(def store-function-queue (r/atom 0))

(defn store-function []
  (reset! store-function-queue 0)
  (let [data (:data @editor-cursor)
        success-callback (fn []
                           (emit [:set-function data])
                           (js/console.info "Data saved"))
        error-callback #(js/console.error "Failed to store data: " %)]
    (put-function {:data data
                   :on-complete success-callback
                   :on-error error-callback})))

(defn debounce-store-function []
  (go
    (swap! store-function-queue inc)
    (<! (timeout 2000))
    (if (> @store-function-queue 1)
      (swap! store-function-queue dec)
      (store-function))))

(defn emit-edit [msg]
  (emit msg handler)
  (debounce-store-function))

(defn delete []
  (let [data (:data @editor-cursor)
        delete-callback #(emit [:delete-function data])]
    (emit [:close-editor nil])
    (delete-function {:function-id (:id data)
                      :on-complete delete-callback})))

(defn close-editor []
  (store-function)
  (emit [:close-editor nil]))

;; -------------------------
;; View

(defn editor-name []
  [:div {:class "editor-section name-editor"}
   [:h3 "Name"]
   [:div {:class "input-wr"}
    [:input
     {:type "text"
      :value (get-in @editor-cursor [:data :name])
      :auto-focus true
      :placeholder "New function name"
      :on-change #(emit-edit [:set-name (.. % -target -value)])}]]])

(defn editor-data-source []
  [:div {:class "editor-section data-source-editor"}
   [:h3 "Data source"]
   [:div {:class "input-wr"}
    [:select
     {:value (get-in @editor-cursor [:data :source])
      :on-change #(emit-edit [:set-source (.. % -target -value)])}
      (for [table (:tables @app-state)]
        [:option {:key (:id table) :value (:id table)}
         (:name table)])]]])

(defn editor-type []
  [:div {:class "editor-section type-editor"}
   [:h3 "Function type"]
   [:div {:class "input-wr"}
    [:select
     {:value (get-in @editor-cursor [:data :type])
      :on-change #(emit-edit [:set-type (.. % -target -value)])}
      (for [[function-key function-type] function-types]
        [:option {:key function-key :value function-key}
         (:label function-type)])]]])

(defn editor-parameters [{:keys [id operand params]}]
  (let [operation-details (get all-operations (keyword operand))
        sort-operations (get-in @editor-cursor [:data :sort-operations])
        operation-index (find-first-i sort-operations id)
        following-operations (into {}
                                   (map #(vector % (get-in @editor-cursor [:data :operations %]))
                                        (subvec sort-operations (inc operation-index))))]
    (case (:type operation-details)
      :source-number
      (let [num-ops (into {}
                          (filter 
                            #(= :number (get-in all-operations [(keyword (:operand (second %))) :result-type]))
                            following-operations))
            options (merge (get-source-number-columns @app-state) num-ops)]
        (-> num-ops clj->js js/console.log)
        (-> options clj->js js/console.log)
        [multiselect {:on-change #(emit-edit [:set-params [id %]])
                      :value params
                      :options options
                      :labelfn :name
                      :ordered true
                      :header "Operator parameters"}])

      :source-string
      [multiselect {:on-change #(emit-edit [:set-params [id %]])
                    :value params
                    :options (merge
                               (get-source-columns @app-state)
                               following-operations)
                    :labelfn :name
                    :ordered true
                    :header "Operator parameters"}]

      :input-number
      [:input {:type "number"
               :value params
               :on-change #(emit-edit [:set-params [id (.. % -target -value)]])}]

      :input-string
      [:input {:type "text"
               :value params
               :on-change #(emit-edit [:set-params [id (.. % -target -value)]])}]

      (do
        (js/console.error (str "failed " (:type operation-details)))
        [:div]))))

(defn editor-operator [id]
  (let [operation (get-in @editor-cursor [:data :operations id])]
    [:tr {:key id}
     [:td
      [:select
       {:value (:operand operation)
        :on-change #(emit-edit [:set-operand [id (.. % -target -value)]])}
       (for [[op-key op] (get-in function-types
                                 [(get-in @editor-cursor [:data :type]) :operations])]
         [:option {:key op-key :value op-key}
          (:label op)])]]
     [:td (editor-parameters operation)]
     [:td
      [:input
       {:type "text"
        :value (:name operation)
        :placeholder "Name"
        :on-change #(emit-edit [:set-label [id (.. % -target -value)]])}]]
     [:td
      [:button
       {:class "btn move-btn"
        :on-click #(emit-edit [:move-back-operation id])}
       "Down"]
      [:button
       {:class "btn move-btn"
        :on-click #(emit-edit [:move-forward-operation id])}
       "Up"]
      [:button
       {:class "btn delete-btn"
        :on-click #(emit-edit [:delete-operation id])}
       "X"]]]))

(defn editor-operations []
  (let [sort-operations (get-in @editor-cursor [:data :sort-operations])
        default-new-label (str "r" (count sort-operations))]
    [:div {:class "output-operations"}
     [:h3 "Operations"]
     [:table
      [:tr
       [:th "Operand"] [:th "Parameters"] [:th "Label"]]
      (doall (map editor-operator sort-operations))]
     [:button
      {:class "btn add-operation-btn"
       :on-click #(emit-edit [:add-operation (init-operation default-new-label)])}
      "Add operation"]]))

(defn editor-output []
  (let [options (merge
                  (get-source-columns @app-state)
                  (get-in @editor-cursor [:data :operations]))]
    [:div {:class "editor-section output-editor-container"}
     [:h3 "Outputs"]
     [multiselect {:on-change #(emit-edit [:set-outputs %])
                   :value (get-in @editor-cursor [:data :outputs])
                   :options options
                   :labelfn :name
                   :header "Outputs"}]]))

(defn render-preview-row [result-row]
  (for [[id value] result-row]
    [:td {:key id} (or value "Blank")]))

(defn editor-preview []
  [:div {:class "editor-section preview"}
   [:h3 "Preview"]
   [:table
    [:tr
     (doall
       (for [output-id (get-in @editor-cursor [:data :outputs])]
         [:th {:key output-id}
          (or (get-in (get-source-columns @app-state) [output-id :name])
              (get-in @editor-cursor [:data :operations output-id :name]))]))]
    (let [results (mapv
                    #(process-row % (:data @editor-cursor))
                    (get-preview-data @app-state))]
      (for [[index result-row] (map-indexed vector results)]
        [:tr {:key index} (render-preview-row result-row)]))]])

(defn function-editor []
  [:div {:class "modal"}
   [:div {:class "modal-header"} "Function Editor"]
   [:div {:class "modal-body"}
    [editor-name]
    [editor-data-source]
    [editor-type]
    [editor-operations]
    [editor-output]
    [editor-preview]]
   [:div {:class "modal-footer"}
    [:button {:class "delete"
              :on-click delete}
     "Delete"]
    [:button {:class "close"
              :on-click close-editor}
     "Close"]]])
