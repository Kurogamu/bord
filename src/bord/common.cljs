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

(defn remove-i [values index]
  (vec
    (concat
      (subvec values 0 index)
      (subvec values (inc index)))))

(defn add-i [values value index]
  (vec
    (concat
      (subvec values 0 index)
      [value]
      (subvec values index))))

(defn move-i [values source-index target-index]
  (if (= source-index target-index)
    values
    (-> values
        (remove-i source-index)
        (add-i (get values source-index) target-index))))

(defn swap-i [values a-index b-index]
  (if (every? values [a-index b-index])
    (let [a-value (nth values a-index)
          b-value (nth values b-index)]
      (-> values
          (assoc a-index b-value)
          (assoc b-index a-value)))
    values))

(defn animation-trigger [animation-name function & args]
  (js/window.addEventListener
    "animationend"
    #(if (= (.-animationName %) animation-name) (apply function args))
    #js {:once true}))

(defn read-number [value]
  (-> value
      (clojure.string/replace #"[^0-9., ]" "")
      (cljs.reader/read-string)))

(defn read-boolean [value]
  (-> value
      (clojure.string/replace #"(true|false)" "")
      (cljs.reader/read-string)))
