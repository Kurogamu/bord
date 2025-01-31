(ns bord.select-modal
  (:require
    [reagent.core :as r]
    ["react" :as react]))

(defn multiselect-modal [{:keys [state on-close options labelfn header ordered value]}]
  (if (not= @state :closed)
    (let
      [available (if ordered
                   options
                   (doall (remove #(contains? @value (first %)) options)) )
       selected (if ordered
                  (doall (map #(vector % (get options %)) @value))
                  (doall (filter #(contains? @value (first %)) options)))]
      [:div {:class (if (= @state :closing) "modal modal-select modal-select-closing" "modal modal-select")}
       [:div {:class "modal-header"}
        [:div {:class "modal-title"} header]
        [:div {:class "modal-menu btn-group"}
         [:button {:class "close" :on-click on-close}
          "Close"]]]
       [:div {:class "modal-body"}
        [:div {:class "available modal-section"}
         [:h3 "Available"]
         (if (some? available)
           [:div {:class "btn-group"}
            (for [[option-key option] available]
              [:button {:class "btn btn-select"
                        :key option-key
                        :on-click #(swap! value conj option-key)}
               (labelfn option)])]
           [:div {:class "blank"} "No options available"])]
        [:div {:class "selected modal-section"}
         [:h3 "Selected"]
         (if (some? selected)
           [:div {:class "btn-group"}
            (for [[index [option-key option]] (map-indexed vector selected)]
              [:button {:class "btn btn-select"
                        :key index
                        :on-click (fn []
                                    (if ordered
                                      (swap! value #(vec (concat (subvec % 0 index) (subvec % (inc index)))))
                                      (swap! value disj option-key)))}
               (labelfn option)])]
           [:div {:class "blank"} "No options selected"])]]])))

(defn multiselect [{:as args :keys [on-change value options labelfn header ordered]}]
  (let [state (r/atom :closed)
        local-value (r/atom (if ordered value (set value)))
        on-close (fn []
                   (js/window.addEventListener
                     "animationend" 
                     (fn [event] (= (.-animationName event) "pop-out")
                        (reset! state :closed)
                        (on-change @local-value))
                     #js {:once true})
                   (reset! state :closing))]
    [:div {:class "input-modal-wrapper"}
     [:button {:class "btn input-modal-btn"
               :on-click #(reset! state :open)}
      (str (count value) " selected...")]
     [multiselect-modal (assoc args :state state :value local-value :on-close on-close)]]))
