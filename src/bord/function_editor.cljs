(ns bord.function-editor
  (:require
    [reagent.core :as r]
    [clojure.string :refer [blank?]]
    [bord.state :refer [app-state emit function-outputs]]
    [bord.common :refer [find-first-i remove-match swap-entry]]
    [bord.data :refer [put-function delete-function]]
    [bord.function :refer [function-types all-operations process-row]]
    [bord.select-modal :refer [multiselect]]
    [cljs.core.async :refer [go timeout]]
    ["react" :as react]))

;; -------------------------
;; Model

(def editor-cursor (r/cursor app-state [:function-editor :function]))

(defn get-source-columns [state]
  (let [source-id (get-in state [:function-editor :function :source])]
    (get-in state [:tables source-id :columns])))

(defn get-source-number-columns [state]
  (into {}
        (filter
          #(= :number (keyword (:type (second %))))
          (get-source-columns state))))

(defn get-preview-data [state]
  (let [source-id (get-in state [:function-editor :function :source])]
    (get-in state [:tables source-id :data-preview])))

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
   :params nil })

;; -------------------------
;; Update

(defn handler [state [event value]]
  (case event
    :set-name (assoc-in state [:function-editor :function :name] value)
    :set-source (assoc-in state [:function-editor :function :source] value)

    :set-type
    (-> state
        (assoc-in [:function-editor :function :operations] {})
        (assoc-in [:function-editor :function :sort-operations] [])
        (assoc-in [:function-editor :function :outputs] [])
        (assoc-in [:function-editor :function :type] value))

    :add-operation 
    (-> state
        (assoc-in [:function-editor :function :operations (:id value)] value)
        (update-in [:function-editor :function :sort-operations] conj (:id value)))

    :move-back-operation 
    (let [index (find-first-i
                  (get-in state [:function-editor :function :sort-operations])
                  value)]
      (update-in state
                 [:function-editor :function :sort-operations]
                 swap-entry index (inc index)))

    :move-forward-operation 
    (let [index (find-first-i
                  (get-in state [:function-editor :function :sort-operations])
                  value)]
      (update-in state
                 [:function-editor :function :sort-operations]
                 swap-entry index (dec index)))

    :delete-operation 
    (-> state
        (update-in [:function-editor :function :operations] dissoc value)
        (update-in [:function-editor :function :sort-operations] remove-match value))

    :set-operand
    (let [[operator operand] value]
      (assoc-in state [:function-editor :function :operations operator :params] [])
      (assoc-in state [:function-editor :function :operations operator :operand] operand))

    :set-params
    (let [[operator params] value]
      (assoc-in state [:function-editor :function :operations operator :params] params))

    :set-label
    (let [[operator label] value]
      (assoc-in state [:function-editor :function :operations operator :name] label))

    :set-outputs
    (assoc-in state [:function-editor :function :outputs] value)

    :set-preview
    (assoc-in state [:function-editor :function :preview] value)

    :init-closing (assoc-in state [:function-editor :closing] true)

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
  (let [data @editor-cursor
        success-callback #(js/console.info "Data saved")
        error-callback #(js/console.error "Failed to store data: " %)]
    (put-function {:data data
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

(defn close-function-editor []
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
  [:div {:class "modal-section name-editor"}
   [:h3 "Name"]
   [:div {:class "input-wrapper"}
    [:input
     {:type "text"
      :value (:name @editor-cursor)
      :auto-focus true
      :placeholder "New function name"
      :on-change #(emit-edit [:set-name (.. % -target -value)])}]]])

(defn editor-data-source []
  [:div {:class "modal-section data-source-editor"}
   [:h3 "Data source"]
   [:div {:class "input-wrapper"}
    [:select
     {:value (:source @editor-cursor)
      :on-change #(emit-edit [:set-source (.. % -target -value)])}
      (for [table (vals (:tables @app-state))]
        [:option {:key (:id table) :value (:id table)}
         (:name table)])]]])

(defn editor-type []
  [:div {:class "modal-section type-editor"}
   [:h3 "Function type"]
   [:div {:class "input-wrapper"}
    [:select
     {:value (:type @editor-cursor)
      :on-change #(emit-edit [:set-type (.. % -target -value)])}
      (for [[function-key function-type] function-types]
        [:option {:key function-key :value function-key}
         (:label function-type)])]]])

(defn editor-parameters [{:keys [id operand params]}]
  (let [operation-details (get all-operations (keyword operand))
        sort-operations (:sort-operations @editor-cursor)
        operation-index (find-first-i sort-operations id)
        following-operations (into {}
                                   (map #(vector % (get-in @editor-cursor [:operations %]))
                                        (subvec sort-operations 0 operation-index)))]
    (case (:type operation-details)
      :source-number
      (let [num-ops (into {}
                          (filter 
                            #(= :number (get-in all-operations [(keyword (:operand (second %))) :result-type]))
                            following-operations))
            options (merge (get-source-number-columns @app-state) num-ops)]
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
      [:div {:class "input-wrapper"}
       [:input {:type "number"
                :value params
                :on-change #(emit-edit [:set-params [id (.. % -target -value)]])}]]

      :input-string
      [:div {:class "input-wrapper"}
       [:input {:type "text"
                :value params
                :on-change #(emit-edit [:set-params [id (.. % -target -value)]])}]]

      (do
        (js/console.error (str "failed " (:type operation-details)))
        [:div]))))

(defn editor-operator [id]
  (let [operation (get-in @editor-cursor [:operations id])]
    [:tr {:key id}
     [:td
      [:select
       {:value (:operand operation)
        :on-change #(emit-edit [:set-operand [id (.. % -target -value)]])}
       (for [[op-key op] (get-in function-types
                                 [(:type @editor-cursor) :operations])]
         [:option {:key op-key :value op-key}
          (:label op)])]]
     [:td (editor-parameters operation)]
     [:td {:class "input-wrapper"}
      [:input
       {:type "text"
        :value (:name operation)
        :placeholder "Name"
        :on-change #(emit-edit [:set-label [id (.. % -target -value)]])}]]
     [:td {:class "btn-group"}
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
  (let [sort-operations (:sort-operations @editor-cursor)
        default-new-label (str "r" (count sort-operations))]
    [:div {:class "modal-section output-operations"}
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
                  (:operations @editor-cursor))]
    [:div {:class "modal-section output-editor-container"}
     [:h3 "Outputs"]
     [multiselect {:on-change #(emit-edit [:set-outputs %])
                   :value (:outputs @editor-cursor)
                   :options options
                   :labelfn :name
                   :header "Outputs"}]]))

(defn editor-preview []
  (let [data (:preview @editor-cursor)]
    [:div {:class "modal-section preview"}
     [:h3 "Preview"]
     (if (and (seq data) (seq (first data)))
       [:div {:class "table-wrapper"}
        [:table
         [:tr
          (doall
            (for [output (function-outputs @editor-cursor)]
              [:th {:key (:id output)} (:name output)]))]
         (for [[index result-row] (map-indexed vector data)]
           [:tr {:key index}
            (for [[id value] result-row]
              [:td {:key id} (or (str value) "Blank")])])]]
       [:div "No data available"])
     ]))

(defn function-editor []
  [:div {:class (if (:closing (:function-editor @app-state))
                  "modal modal-editor modal-editor-closing"
                  "modal modal-editor")}
   [:div {:class "modal-header"} 
    [:div {:class "modal-title"} "Function Editor"]
    [:div {:class "modal-menu btn-group"}
     [:button {:class "btn delete-btn"
               :on-click delete}
      "Delete"]
     [:button {:class "btn close-btn"
               :on-click close-function-editor}
      "Close"]]]
   [:div {:class "modal-body"}
    [editor-name]
    [editor-data-source]
    [editor-type]
    [editor-operations]
    [editor-output]
    [editor-preview]]])
