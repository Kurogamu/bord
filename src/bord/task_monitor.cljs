(ns bord.task-monitor
  (:require
    [bord.state :refer [emit app-state]]))

(def task-states
  [:running :queued :completed :failed])

(def task-symbol 
  {:running "\u23F5"
   :queued "\u23F8"
   :completed "\u2713"
   :failed "\u2717"})

(defn task-info [task]
  (let [[event value] (:msg task)]
    (case event
      :upload-table
      (let [table-meta
            (get-in @app-state [:tables (second value)])]
        {:key (:id table-meta)
         :description (str "Upload table " (:name table-meta))
         :progress (or (:progress task) nil)})
      {})))

(defn task-summary []
  [:div
   {:class "task-summary"
    :on-click #(if (some? (:task-viewer @app-state))
                 (emit [:show-tasks nil])
                 (emit [:show-tasks :running]))}
   [:div.description "Task summary"]
   (doall
     (map
       (fn [state-key]
         [:div
          {:key state-key
           :class "counter"}
          [:span.number (count (get-in @app-state [:tasks state-key]))]
          [:span.symbol (get task-symbol state-key)]])
       task-states))])

(defn task-view [task]
  (let [info (task-info task)]
    [:div
     {:key (:key info)
      :class "task"}
     [:div.description (:description info)]
     [:div.progress
      (condp = (:progress info)
        nil "Not started"
        1 "Completed"
        (str (* (:progress info) 100) "%"))]]))

(defn task-view-popup []
  (let [selected (get-in @app-state [:task-viewer :state])
        tasks (get-in @app-state [:tasks selected])]
    [:div
     {:class
      (if (:closing (:task-viewer @app-state))
        "popup popup-closing task-viewer" "popup task-viewer")}
     [:div.tab-selector-container
      (map
        (fn [tab-key]
          [:button
           {:key tab-key
            :class (if (= selected tab-key)
                     "tab-selector tab-selector-active"
                     "tab-selector")
            :on-click #(emit [:show-tasks tab-key])}
           (clj->js tab-key)])
        task-states)]
     (if (seq tasks)
       [:div.task-list (doall (map task-view tasks))]
       [:div.task-list-empty (str "No " (clj->js selected) " tasks found")])]))
