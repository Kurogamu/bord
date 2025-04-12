(ns bord.function
  (:require
    [bord.common :refer [read-number read-boolean]]))

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

(defn cast-params [params param-type]
  (map
    (fn [param]
      (case param-type
        :source-boolean (read-boolean param)
        :source-number (read-number param)
        param))
    params))

(defn run-operation [operation data-sets]
  (let [operand (keyword (:operand operation))
        params (mapcat
                 (fn [data] (map #(get data %) (:params operation)))
                 data-sets)
        values (cast-params params (param-type operand))]
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

(defn process-row [operations input-row & additional-inputs]
  (reduce
    (fn [row-values [operation-id operation]]
      (assoc
        row-values
        operation-id
        (run-operation operation (cons row-values additional-inputs))))
    input-row
    operations))

(defn run-map [data function]
  (mapv
    (fn [data-row]
      (-> (process-row (:operations function) data-row)
          (select-keys (:outputs function))))
    data))

(defn run-filter [data function]
  (->> data
       (mapv #(process-row (:operations function) %))
       (filter #(get % (last (:sort-operations function))))
       (mapv #(select-keys % (:outputs function)))))

(defn run-reduce [data function]
  (let [operations-with-self
        (update-vals
          (:operations function)
          (fn [op] (update op :params conj (:id op))))]
    (->
      (reduce
        (fn [result-row input-row]
          (select-keys 
            (process-row operations-with-self input-row result-row)
            (keys (:operations function))))
        {}
        data)
      (select-keys (:outputs function))
      (vector))))

(defn run-function [data function]
  (case (keyword (:type function))
    :map (run-map data function)
    :filter (run-filter data function)
    :reduce (run-reduce data function)))
