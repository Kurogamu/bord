(ns bord.function-process-handler
  (:require
    [bord.state :refer [emit app-state]]
    [bord.data :as data]
    [bord.function :refer [run-function]]
    [bord.worker-handler :as worker]))

(defn offsets [num-fragments]
  (let [limit (js/Math.ceil (/ num-fragments worker/num-workers))]
    (map
      (fn [index]
        [(* index limit)
         limit])
      (range worker/num-workers))))

(defn gather-reduce [function results on-success]
  (let [sorted-results (->> results
                            (map :result)
                            (sort-by :offset)
                            (map #(-> % :value first))
                            (map #(assoc % "b8170aeb-0a72-48f7-b4be-4c1245774e57" 1)))
        reduce-result (run-function sorted-results function)]
    (js/console.log (clj->js function))
    (js/console.log (clj->js sorted-results))
    (js/console.log (clj->js reduce-result))
    (data/put-fragment
      {:data {:id (js/crypto.randomUUID)
              :object-id (:id function)
              :first-row 0
              :last-row 1
              :offset 0
              :data reduce-result}
       :on-success
       (fn []
         (js/console.info "Pushed gathered results ")
         (on-success))
       :on-error
       #(js/console.error "Error pushing gathered results")})))

(defn process-gathered [function results]
  (letfn [(on-success []
            (emit [:set-function-state [(:id function) :processed]]))]
    (if (= (keyword (:type function)) :reduce)
      (do
        (gather-reduce function results on-success)
        (on-success))
      (on-success))))

(defn gather-results [msg function num-fragments gathered-results]
  (swap! gathered-results conj msg)
  (when (=
         (count @gathered-results)
         (count (offsets num-fragments)))
    (process-gathered function @gathered-results)))

(defn spec-jobs [function-id [offset end]]
  [:process-function [function-id offset end]])

(defn trigger-process [function-id]
  (emit [:set-function-state [function-id :processing]])
  (let [function (get-in @app-state [:functions function-id])
        gathered-results (atom [])]
    (data/delete-fragments
      {:object-id function-id
       :on-success
       (fn []
         (data/count-fragments
           {:object-id (:source function)
            :on-success
            (fn [num-fragments]
              (worker/set-result-callback
                function-id
                #(gather-results
                   %
                   function
                   num-fragments
                   gathered-results))
              (doall
                (->> (offsets num-fragments)
                     (map #(spec-jobs function-id %))
                     (map worker/worker-emit))))}))})))
