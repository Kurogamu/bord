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
        :columns
        vals
        )))

(defn init-function-data []
  {:id (js/crypto.randomUUID)
   :name ""
   :created (js/Date.now)
   :updated (js/Date.now)
   :sort-outputs []
   :outputs {}
   :type :map
   :formula ""})

(def function-types
  [{:key :filter
    :label "Filter"}
   {:key :map
    :label "Map"}
   {:key :reduce
    :label "Reduce"}])

(def text-filters
  [{:key :equals
    :label "="}
   {:key :not-equals
    :label "U+2260"}
  {:key :contains
    :label "Contains"}
  {:key :not-contains
    :label "Not in"}])

(defn init-operation [label]
  {:id (js/crypto.randomUUID)
   :operand :vector
   :label (or label "")
   :params #{} })

;; -------------------------
;; Update

(defn handler [state [event value]]
  (-> state clj->js js/console.log)
  (case event
    :set-name (assoc-in state [:function-editor :data :name] value)
    :set-type (assoc-in state [:function-editor :data :type] value)
    :set-source (assoc-in state [:function-editor :data :source] value)

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

    :set-label
    (let [[operator label] value]
      (assoc-in state [:function-editor :data :operations operator :label] label))

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

(defn editor-type []
  [:div {:class "editor-section type-editor"}
   [:h3 "Function type"]
   [:div {:class "input-wr"}
    [:select
     {:value (get-in @editor-cursor [:data :type])
      :on-change #(emit-edit [:set-type (.. % -target -value)])}
      (for [function-type function-types]
        [:option {:key (:key function-type) :value (:key function-type)}
         (:label function-type)])]]])

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

(defn editor-operator [id]
  (let [operation (get-in @editor-cursor [:data :operations id])]
    [:tr {:key id}
     [:td
      [:select
       {:value (:operand operation)
        :on-change #(emit-edit [:set-operand [id (.. % -target -value)]])}
       (for [op text-filters]
         [:option {:key (:key op) :value (:key op)}
          (:label op)])]]
     [:td
      [multiselect {:on-change #(emit-edit [:set-params [id %]])
                    :value (:params operation)
                    :options (get-source-columns @app-state)
                    :keyfn :id
                    :labelfn :name
                    :header "Operator parameters"}]]
     [:td
      [:input
       {:type "text"
        :value (:label operation)
        :placeholder "Label"
        :on-change #(emit-edit [:set-label [id (.. % -target -value)]])}]]
     ]))

(defn editor-operations []
  [:div {:class "output-operations"}
   [:table
    [:tr
     [:th "Operand"] [:th "Parameters"] [:th "Result"]]
    (doall
      (for [operation-id (get-in @editor-cursor [:data :sort-operations])]
        (editor-operator operation-id)))]
   [:button {:class "btn add-operation-btn"
             :on-click #(emit-edit [:add-operation (init-operation (str "r" (count (get-in @editor-cursor [:data :sort-operations]))))])}
    "Add operation"]])

(defn editor-output []
  [:div {:class "editor-section output-editor-container"}
   [:h3 "Outputs"]
   [:div {:class "outputs"}
    (doall
      (for [output-id (get-in @editor-cursor [:data :sort-outputs])]
        (editor-output output-id)))]])

(defn function-editor []
  [:div {:class "modal"}
   [:div {:class "modal-header"} "Function Editor"]
   [:div {:class "modal-body"}
    [editor-name]
    [editor-type]
    [editor-data-source]
    [editor-operations]
    [editor-output]]
   [:div {:class "modal-footer"}
    [:button {:class "delete"
              :on-click delete}
     "Delete"]
    [:button {:class "close"
              :on-click close-editor}
     "Close"]]])
