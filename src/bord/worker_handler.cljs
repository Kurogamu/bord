(ns bord.worker-handler
  (:require
    [bord.state :refer [app-state emit]]
    [clojure.edn :refer [read-string]]))

(defonce workers (atom {}))

(def num-workers 2)

(defn- state-handler [state [event value worker-id]]
  (case event
    :set-task
    (assoc-in state [:tasks :running worker-id] {:msg value})
    :queue-task
    (update-in state [:tasks :queued] conj value)
    :set-progress
    (if (< value 1)
      (assoc-in state [:tasks :running worker-id :progress] value)
      (-> state
          (assoc-in [:tasks :running worker-id :progress] 1)
          (update-in [:tasks :completed]
                     conj (get-in state [:tasks :running worker-id]))
          (update-in [:tasks :running] dissoc worker-id)))
    state))

(defn- available-workers []
  (clojure.set/difference
    (set (keys @workers))
    (set (keys (:running (:tasks @app-state))))))

(defn worker-emit [msg]
  (if-let [worker-id (first (available-workers))]
    (do
      (emit [:set-task msg worker-id] state-handler)
      (.postMessage (get @workers worker-id) (pr-str msg)))
    (emit [:queue-task msg nil] state-handler)))

(defn- message-handler [e worker-id]
  (let [[event value] (read-string (.-data e))]
    (if (= event :worker-init)
      (js/console.info (str "registered worker " worker-id ": " value)))
    (emit [event value worker-id] state-handler)))

(defn init-workers []
  (doall
    (for [id (range num-workers)]
      (let [worker (js/Worker. "js/worker.js")]
        (swap! workers assoc id worker)
        (.addEventListener worker "message" #(message-handler % id))))))
