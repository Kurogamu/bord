(ns bord.function-editor
  (:require
    [reagent.core :as r]
    [clojure.string :refer [blank?]]
    [bord.state :refer [app-state emit]]
    [bord.data :refer [put-function delete-function]]
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

(defn init-function-data []
  {:id (js/crypto.randomUUID)
   :name ""
   :created (js/Date.now)
   :updated (js/Date.now)
   :outputs []
   :type :map
   :formula ""})

(def const-operations
  [{:key :number-constant
    :label "Numeric Constant"
    :type :input-number}
   {:key :text-constant
      :label "Text Constant"
      :type :input-text}])

(def map-operations
  [{:key :sum
    :label "Sum"
    :type :source-number}
   {:key :negative
    :label "Negative"
    :type :source-number}
   {:key :product
    :label "Product"
    :type :source-number}
   {:key :inverse
    :label "Inverse"
    :type :source-number}
   {:key :format
    :label "Format"
    :type :source-text}])

(def filter-operations
  [{:key :equals
    :label "Equals"
    :type :source-text}
   {:key :not-equals
    :label "Not equal"
    :type :source-text}
   {:key :contains
    :label "Contains"
    :type :source-text}
   {:key :not-contains
    :label "Not containing"
    :type :source-text}])

(def reduce-operations
  [{:key :sum
    :label "Sum"
    :type :source-number}
   {:key :negative
    :label "Negative"
    :type :negative}
   {:key :concat
    :label "Concat"
    :type :source-text}
   {:key :concat-unique
    :label "Concat unique"
    :type :source-text}])

(def function-types
  {"map" {:label "Map"
          :operations map-operations}
   "filter" {:label "Filter"
             :operations filter-operations}
   "reduce" {:label "Reduce"
             :operations reduce-operations}})

(def text-filters
  [{:key :equals
    :label "="}
   {:key :not-equals
    :label "U+2260"}
  {:key :contains
    :label "Contains"}
  {:key :not-contains
    :label "Not in"}])

(defn init-operation [name]
  {:id (js/crypto.randomUUID)
   :operand nil
   :name (or name "")
   :constant ""
   :params #{} })

;; -------------------------
;; Update

(defn handler [state [event value]]
  (-> state clj->js js/console.log)
  (case event
    :set-name (assoc-in state [:function-editor :data :name] value)
    :set-source (assoc-in state [:function-editor :data :source] value)

    :set-type
    (-> state
        (assoc-in [:function-editor :data :operations] {})
        (assoc-in [:function-editor :data :sort-operations] [])
        (assoc-in [:function-editor :data :type] value))

    :add-operation 
    (-> state
        (assoc-in [:function-editor :data :operations (:id value)] value)
        (update-in [:function-editor :data :sort-operations] #(conj % (:id value))))

    :set-operand
    (let [[operator operand] value]
      (assoc-in state [:function-editor :data :operations operator :operand] operand))

    :set-params
    (let [[operator params] value]
      (assoc-in state [:function-editor :data :operations operator :params] params))

    :set-constant
    (let [[operator constant] value]
      (assoc-in state [:function-editor :data :operations operator :constant] constant))

    :set-operation-name
    (let [[operator name] value]
      (assoc-in state [:function-editor :data :operations operator :name] name))

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

(defn editor-parameters [id operation]
  [multiselect {:on-change #(emit-edit [:set-params [id %]])
                :value (:params operation)
                :options (merge
                           (get-source-columns @app-state)
                           (get-in @editor-cursor [:data :operations]))
                :labelfn :name
                :ordered true
                :header "Operator parameters"}])

(defn editor-operator [id]
  (let [operation (get-in @editor-cursor [:data :operations id])]
    [:tr {:key id}
     [:td
      [:select
       {:value (:operand operation)
        :on-change #(emit-edit [:set-operand [id (.. % -target -value)]])}
       (for [op 
             (concat 
               (get-in function-types [(get-in @editor-cursor [:data :type]) :operations])
               const-operations)]
         [:option {:key (:key op) :value (:key op)}
          (:label op)])]]
     [:td
      [multiselect {:on-change #(emit-edit [:set-params [id %]])
                    :value (:params operation)
                    :options (merge
                               (get-source-columns @app-state)
                               (get-in @editor-cursor [:data :operations]))
                    :labelfn :name
                    :ordered true
                    :header "Operator parameters"}]]
     [:td
      [:input
       {:type "text"
        :value (:name operation)
        :placeholder "Name"
        :on-change #(emit-edit [:set-operation-name [id (.. % -target -value)]])}]]]))

(defn editor-operations []
  [:div {:class "output-operations"}
   [:h3 "Operations"]
   [:table
    [:tr
     [:th "Operand"] [:th "Constant"] [:th "Parameters"] [:th "Label"]]
    (doall
      (for [operation-id (get-in @editor-cursor [:data :sort-operations])]
        (editor-operator operation-id)))]
   [:button {:class "btn add-operation-btn"
             :on-click #(emit-edit [:add-operation (init-operation (str "r" (count (get-in @editor-cursor [:data :sort-operations]))))])}
    "Add operation"]])

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

(defn editor-preview []
  [:div {:class "editor-section preview"}
   [:h3 "Preview"]
   [:table
    [:tr
     (doall
       (for [output-id (get-in @editor-cursor [:data :outputs])]
       [:th {:key output-id}
        (or (get-in (get-source-columns @app-state) [output-id :name]) (get-in @editor-cursor [:data :operations output-id :name]))]))]]])

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
