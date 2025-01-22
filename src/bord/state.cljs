(ns bord.state
  (:require
    [reagent.core :as r]))

;; ----------------
;; Model

(def default-state
  {:tables []
   :table-editor nil
   :function-editor nil
   :tables-loading false
   :functions-loading false})

(defonce app-state (r/atom default-state))

(defn shared-handler [state [event value]]
  (case event
    :set-table (let [table-index 
                     (->> (:tables state)
                          (map-indexed vector)
                          (filter #(= (:id (second %)) (:id value)))
                          first
                          first)]
                 (if (some? table-index)
                   (assoc-in state [:tables table-index] value)
                   (update-in state [:tables] #(conj % value))))
    :delete-table (assoc state :tables (filterv #(not= (:id %) (:id value)) (:tables state)))
    :set-tables (assoc state :tables (vec value) :tables-loading false)
    :set-tables-loading (assoc state :tables-loading value)
    :set-functions (assoc state :functions (vec value) :functions-loading false)
    :set-functions-loading (assoc state :functions-loading value)
    :open-editor-table (assoc state :table-editor {:meta value})
    :set-editor-table (assoc-in state [:table-editor :meta] value)
    :close-editor (assoc state :table-editor nil :function-editor nil)
    :open-editor-function (assoc state :function-editor {:data value})
    :set-editor-function (assoc-in state [:function-editor :data] value)
    state))

(defn emit 
  ([msg] (r/rswap! app-state shared-handler msg))
  ([msg handler] (r/rswap! app-state handler msg)))
