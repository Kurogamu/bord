(ns bord.select-modal
  (:require
    [reagent.core :as r]
    ["react" :as react]))

(defn multiselect-modal [{:keys [on-change options labelfn header ordered active value]}]
  (let
    [available (if ordered
                 options
                 (doall (remove #(contains? @value (first %)) options)) )
     selected (if ordered
                (doall (map #(vector % (get options %)) @value))
                (doall (filter #(contains? @value (first %)) options)))]
    [:div {:class (if @active "modal input-modal" "input-modal-inactive")}
     [:div {:class "modal-header"} header]
     [:div {:class "available"}
      [:h3 "Available"]
      (if (some? available)
        (for [[option-key option] available]
          [:button {:class "btn btn-select"
                    :key option-key
                    :on-click #(swap! value conj option-key)}
           (labelfn option)])
        [:div {:class "blank"} "No options available"])]
     [:div {:class "selected"}
      [:h3 "Selected"]
      (if (some? selected)
        (for [[index [option-key option]] (map-indexed vector selected)]
          [:button {:class "btn btn-select"
                    :key index
                    :on-click (fn []
                                (if ordered
                                  (swap! value #(vec (concat (subvec % 0 index) (subvec % (inc index)))))
                                  (swap! value disj option-key)))}
           (labelfn option)])
        [:div {:class "blank"} "No options selected"])]
     [:div {:class "modal-footer"}
      [:button {:class "close"
                :on-click #(do
                             (reset! active false)
                             (on-change @value))}
       "Close"]]]))

(defn multiselect [{:as args :keys [on-change value options labelfn header ordered]}]
  (let [active (r/atom false)
        local-value (r/atom (if ordered value (set value)))]
    [:div {:class "input-modal-wrapper"}
     [:button {:class "btn input-modal-btn"
               :on-click #(swap! active not)}
      (str (count value) " selected...")]
     [multiselect-modal (assoc args
                               :active active
                               :value local-value)]]))
