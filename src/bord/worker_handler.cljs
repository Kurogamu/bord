(ns bord.worker-handler
  (:require
    [bord.state :refer [app-state emit]]
    [clojure.edn :refer [read-string]]))

(defonce workers (atom {}))

(def num-workers 4)

(defn- state-handler [state [event value worker-id]]
  (case event
    :set-task
    (assoc-in state [:tasks :running worker-id] {:msg value})
    :queue-task
    (update-in state [:tasks :queued] conj value)
    :dequeue-first
    (update-in state [:tasks :queued] rest)
    :set-progress
    (assoc-in state [:tasks :running worker-id :progress] value)
    :set-result
    (-> state
      (update-in
        [:tasks :completed]
        conj 
        (assoc (get-in state [:tasks :running worker-id]) :progress 1))
      (update-in [:tasks :running] dissoc worker-id))
    :set-gather
    (assoc-in state [:tasks :on-result (first value)] (second value))
    state))

(defn set-result-callback [target on-result]
  (emit [:set-gather [target on-result] nil] state-handler))

(defn process-results [value]
  (if-let [on-result (get-in @app-state [:tasks :on-result (:target value)])]
    (on-result value)))

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

(defn dequeue []
  (when-let [queued (-> @app-state :tasks :queued first)]
    (emit [:dequeue-first nil nil] state-handler)
    (worker-emit queued)))

(defn- message-handler [e worker-id]
  (let [[event value] (read-string (.-data e))
        default #(emit [event value worker-id] state-handler)]
    (case event
      :worker-init
      (js/console.info (str "registered worker " worker-id ": " value))
      :set-result
      (do
        (default)
        (dequeue)
        (process-results value))
      (default))))

(defn init-workers []
  (doall
    (for [id (range num-workers)]
      (let [worker (js/Worker. "js/worker.js")]
        (swap! workers assoc id worker)
        (.addEventListener worker "message" #(message-handler % id))))))
