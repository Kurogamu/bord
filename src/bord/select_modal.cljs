(ns bord.select-modal
  (:require
    [reagent.core :as r]
    ["react" :as react]))

(defn multiselect-modal [{:keys [on-change options keyfn labelfn header active value]}]
  (let
    [available (doall (remove #(contains? @value (keyfn %)) options))
     selected (doall (filter #(contains? @value (keyfn %)) options))]
    [:div {:class (if @active "modal input-modal" "input-modal-inactive")}
     [:div {:class "modal-header"} header]
     [:div {:class "available"}
      [:h3 "Available"]
      (if (some? available)
        (for [option available]
          [:button {:class "btn btn-select"
                    :key (keyfn option)
                    :on-click #(swap! value conj (keyfn option))}
           (labelfn option)])
        [:div {:class "blank"} "No options available"])]
     [:div {:class "selected"}
      [:h3 "Selected"]
      (if (some? selected)
        (for [option selected]
          [:button {:class "btn btn-select"
                    :key (keyfn option)
                    :on-click #(swap! value conj (keyfn option))}
           (labelfn option)])
        [:div {:class "blank"} "No options selected"])]
     [:div {:class "modal-footer"}
      [:button {:class "close"
                :on-click #(do
                             (reset! active false)
                             (on-change @value))}
       "Close"]]]))

(defn multiselect [{:as args :keys [on-change value options keyfn labelfn header]}]
  (let [active (r/atom false)
        local-value (r/atom (set value)) ]
    [:div {:class "input-modal-wrapper"}
     [:button {:class "btn input-modal-btn"
               :on-click #(swap! active not)}
      (str (count value) " selected...")]
     [multiselect-modal (assoc args
                               :active active
                               :value local-value)]]))
