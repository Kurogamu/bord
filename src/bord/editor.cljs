(ns bord.editor
  (:require
    [reagent.core :as r]
    [reagent.dom :as d]
    ["react" :as react]))

(defn table-editor-default-state []
  {:new true
   :name ""
   :columns []
   :count 0})

(defonce table-editor-state (r/atom nil))


(defn table-editor-init-state
  ([] (reset! table-editor-state (table-editor-default-state)))
  ([data] (reset! table-editor-state {:new false
                                      :tableId (.-tableId data)
                                      :name (.-name data)
                                      :columns []
                                      :count 0})))


(defn table-data-payload [state]
  {:tableId (:tableId state)
   :name (:name state)
   :created (if (:new state) (js/Date.now) (:created state))
   :updated (js/Date.now)
   :columns (:columns state)
   :count (:count state)})


(defn table-editor [{:keys [on-save on-close]}]
  (let [new-table (:new @table-editor-state)
        save-text (if new-table "Create" "Update")]
    [:div {:class "modal"}
     [:div {:class "modal-header"} "Table Editor"]
     [:div {:class "modal-body"}
      [:div {:class "table-editor"}
       [:div {:class "input-wr"}
        [:input
         {:type "text"
          :value (:name @table-editor-state)
          :auto-focus true
          :placeholder "New table name"
          :on-change #(swap! table-editor-state assoc :name (.. % -target -value)) }
         ]
        ]
       ]
      ]
     [:div {:class "modal-footer"}
      [:button {:class "cancel"
                :on-click on-close}
       "Cancel"]
      [:button {:class "save"
                :on-click #(on-save (table-data-payload @table-editor-state))}
       save-text]]]))
