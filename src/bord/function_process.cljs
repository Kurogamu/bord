(ns bord.function-process
  (:require
    [bord.state :refer [app-state]]
    [bord.data :as data]
    [bord.function :refer [run-function]]
    [bord.worker-handler :refer [worker-emit num-workers]]))

(defn offsets [num-fragments]
  (let [limit (js/Math.ceil (/ num-fragments num-workers))]
    (map
      (fn [index]
        [(* index limit)
         limit])
      (range num-workers))))

(defn spec-jobs [function-id [offset end]]
  [:process-function [function-id offset end]])

(defn trigger-process [function-id]
  (let [function (get-in @app-state [:functions function-id])]
    (data/delete-fragments
      {:table-id function-id
       :on-complete
       (fn []
         (data/count-fragments
           {:table-id (:source function)
            :on-complete
            (fn [num-fragments]
              (doall
                (->> (offsets num-fragments)
                     (map #(spec-jobs function-id %))
                     (map worker-emit))))}))})))

(defn result-fragment [args]
  (assoc args :id (js/crypto.randomUUID)))

(defn process-fragment [function value report-progress]
  (if value
    (let [results (doall (run-function (:data value) function))
          result-fragment {:id (js/crypto.randomUUID)
                           :table-id (:id function)
                           :first-row (:first-row value)
                           :last-row (:last-row value)
                           :offset (:offset value)
                           :data results}]
      (js/console.log "Pushing fragment..." (:offset result-fragment))
      (data/put-fragment
        {:data result-fragment
         :on-success
         #(js/console.info "Pushed fragment " (:offset result-fragment))
         :on-error
         #(js/console.error "Error fragment " (:offset value))})
      (report-progress (:offset value))
      true)
    (report-progress nil)))

(defn process-fragments
  [{:keys [function-id fragment-offset limit on-progress]}]
  (letfn [(report-progress [offset]
            (if (some? offset)
              (/ (- offset fragment-offset) limit)
              (on-progress 1)))]
    (data/fetch-function
      {:function-id function-id
       :on-complete
       (fn [function]
         (on-progress 0)
         (data/read-table-fragments
           {:table-id (:source function)
            :offset fragment-offset
            :limit limit
            :cursor-callback #(process-fragment function % report-progress)
            :on-complete #(js/console.info "Fragment processing complete")
            :on-error #(js/console.error "Fragment processing error")}))})))
