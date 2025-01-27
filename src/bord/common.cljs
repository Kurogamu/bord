(ns bord.common)

(def keycodes
  {:enter 13
   :esc 27
   :tab 9})

(defn find-first-i [values value]
  (loop [s values
         i 0]
    (cond
      (not s) -1
      (= (first s) value) i
      :else (recur (next s) (inc i)))))

(defn remove-match [values value]
  (vec (remove #(= value %) values)))

(defn swap-entry [values a-index b-index]
  (if (every? values [a-index b-index])
    (let [a-value (nth values a-index)
          b-value (nth values b-index)]
      (-> values
          (assoc a-index b-value)
          (assoc b-index a-value)))
    values))
