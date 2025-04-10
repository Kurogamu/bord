(ns bord.function
  (:require
    [bord.common :refer [read-number read-boolean]]
    [reagent.core :as r]))

(def const-operations
  {:number-constant {:label "Numeric Constant"
                     :param-type :input-number
                     :result-type :number}
   :string-constant {:label "Text Constant"
                     :param-type :input-string
                     :result-type :string}})

(def map-operations
  {:sum {:label "Sum"
         :param-type :source-number
         :result-type :number}
   :subtract {:label "Subtract"
              :param-type :source-number
              :result-type :number}
   :product {:label "Product"
             :param-type :source-number
             :result-type :number}
   :divide {:label "Divide"
             :param-type :source-number
             :result-type :number}})

(def filter-operations
  {:equals {:label "Equals"
            :param-type :source-string
            :result-type :boolean}
   :not-equals {:label "Not equal"
                :param-type :source-string
                :result-type :boolean}
   :contains {:label "Contains"
              :param-type :source-string
              :result-type :boolean}
   :not-contains {:label "Not containing"
                  :param-type :source-string
                  :result-type :boolean}
   :or {:label "Or"
        :param-type :source-boolean
        :result-type :boolean}
   :and {:label "And"
         :param-type :source-boolean
         :result-type :boolean}})

(def reduce-operations
  {:sum {:label "Sum"
         :param-type :source-number
         :result-type :number}
   :negative {:label "Negative"
              :param-type :negative
              :result-type :number}
   :concat {:label "Concat"
            :param-type :source-string
            :result-type :string}})

(def all-operations
  (merge const-operations map-operations filter-operations reduce-operations))

(defn param-type [operation]
  (get-in all-operations [operation :param-type]))

(defn result-type [operation]
  (get-in all-operations [operation :result-type]))

(def function-types
  {:map {:label "Map"
         :description "Apply to each row"
         :operations (merge map-operations const-operations)}
   :filter {:label "Filter"
            :description "Select some rows"
            :operations (merge filter-operations const-operations)}
   :reduce {:label "Reduce"
            :description "Squash all rows into one"
            :operations (merge reduce-operations const-operations)}})

(defn run-operation [data operation]
  (let [operand (keyword (:operand operation))
        params (map #(get data %) (:params operation))
        values (map
                 (fn [param]
                   (case (param-type operand)
                     :source-boolean (read-boolean param)
                     :source-number (read-number param)
                     param))
                 params)]
    (case operand
      :number-constant (read-number (:params operation))
      :string-constant (:params operation)
      :sum (apply + values)
      :subtract (apply - values)
      :product (apply * values)
      :divide (apply / values)
      :equals (apply = values)
      :not-equals (apply not= values)
      :contains (every? #(contains? (first values) %) (rest values))
      :not-contains (not-every? #(contains? (first values) %) (rest values))
      :or (some true? values)
      :and (every? identity values)
      :concat (apply conj values)
      nil)))

(defn process-row [input-row operations]
  (reduce 
    (fn [results [operation-id operation]]
      (assoc results operation-id (run-operation results operation)))
    input-row
    operations))

(defn run-map [data function]
  (mapv 
    (fn [data-row]
      (-> data-row
          (process-row (:operations function))
          (select-keys (:outputs function))))
    data))

(defn run-filter [data function]
  (->> data
       (mapv #(process-row % (:operations function)))
       (filter #(get % (last (:sort-operations function))))
       (mapv #(select-keys % (:outputs function)))))

(defn run-reduce [data function]
  (let [operations (reduce 
                     (fn [result [id op]]
                       (assoc
                         result
                         id (update op :params #(cons id %))))
                     {}
                     (:operations function))
        init-values (update-vals (:operations function) (fn [op] 0))]
    (->
      (reduce
        #(-> (merge %1 %2) (process-row operations))
        init-values
        data)
      (select-keys (:outputs function))
      (vector))))

(defn run-function [data function]
  (case (keyword (:type function))
    :map (run-map data function)
    :filter (run-filter data function)
    :reduce (run-reduce data function)))
