(ns bord.function-process
  (:require
    [bord.data :as data]
    [bord.function :refer [run-function]]))

(defn result-fragment [args]
  (assoc args :id (js/crypto.randomUUID)))

(defn put-result-fragment [function input results]
  (let [result-fragment {:id (js/crypto.randomUUID)
                         :object-id (:id function)
                         :first-row (:first-row input)
                         :last-row (:last-row input)
                         :offset (:offset input)
                         :data results}]
    (data/put-fragments
      {:data [result-fragment]
       :on-success
       #(js/console.info "Pushed fragment " (:offset result-fragment))
       :on-error
       #(js/console.error "Error fragment " (:offset input))})))

(defn process-fragment [function value on-report partial-result]
  (if value
    (let [reduce-type (= (-> function :type keyword) :reduce)
          input (if (and reduce-type (some? @partial-result))
                  (cons (first @partial-result) (:data value))
                  (:data value))
          results (doall (run-function input function))]
      (if reduce-type
        (reset! partial-result results)
        (put-result-fragment function value results))
      (on-report (:offset value))
      true)
    (on-report)))

(defn process-fragments
  [{:keys [function-id fragment-offset limit report]}]
  (let [partial-result (atom nil)
        on-report
        (fn
          ([offset]
           (report [:set-progress (/ (- offset fragment-offset) limit)]))
          ([]
           (report
             [:set-result
              {:target function-id
               :result {:offset fragment-offset :value @partial-result}}])))
        read-fragments
        (fn [function]
          (report [:set-progress 0])
          (data/read-fragments
            {:object-id (:source function)
             :offset fragment-offset
             :limit limit
             :cursor-callback
             #(process-fragment function % on-report partial-result)}))]
    (data/fetch-function
      {:function-id function-id
       :on-complete read-fragments})))
