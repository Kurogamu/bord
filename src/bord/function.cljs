(ns bord.function
  (:require
    [reagent.core :as r]))

(def const-operations
  {:number-constant {:label "Numeric Constant"
                     :type :input-number
                     :result-type :number}
   :string-constant {:label "Text Constant"
                   :type :input-string
                   :result-type :string}})

(def map-operations
  {:sum {:label "Sum"
         :type :source-number
         :result-type :number}
   :subtract {:label "Subtract"
              :type :source-number
              :result-type :number}
   :product {:label "Product"
             :type :source-number
             :result-type :number}
   :divide {:label "Divide"
             :type :source-number
             :result-type :number}
   :format {:label "Format"
            :type :source-string
            :result-type :string}})

(def filter-operations
  {:equals {:label "Equals"
            :type :source-string
            :result-type :boolean}
   :not-equals {:label "Not equal"
                :type :source-string
                :result-type :boolean}
   :contains {:label "Contains"
              :type :source-string
              :result-type :boolean}
   :not-contains {:label "Not containing"
                  :type :source-string
                  :result-type :boolean}
   :or {:label "Or"
        :type :boolean
        :result-type :boolean}
   :and {:label "And"
         :type :boolean
         :result-type :boolean}})

(def reduce-operations
  {:sum {:label "Sum"
         :type :source-number
         :result-type :number}
   :negative {:label "Negative"
              :type :negative
              :result-type :number}
   :concat {:label "Concat"
            :type :source-string
            :result-type :string}})

(def all-operations
  (merge const-operations map-operations filter-operations reduce-operations))

(def function-types
  {"map" {:label "Map"
          :operations (merge map-operations const-operations)}
   "filter" {:label "Filter"
             :operations (merge filter-operations const-operations)}
   "reduce" {:label "Reduce"
             :operations (merge reduce-operations const-operations)}})

(defn run-operation [data operation]
  (let [values (map #(get data %) (:params operation))]
    (case (keyword (:operand operation))
      :number-constant (:params operation)
      :string-constant (:params operation)
      :sum (apply + values)
      :subtract (apply - values)
      :product (apply * values)
      :divide (apply / values)
      :format (first values)
      nil)))

(defn process-row [row-data function]
  (let [results (reduce 
                  (fn [data operation]
                    (assoc data (:id operation) (run-operation data operation)))
                  row-data
                  (vals (:operations function)))]
    (mapv #(vector % (get results %)) (:outputs function))))
