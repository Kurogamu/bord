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
    :set-table (assoc-in state [:tables (:id value)] value)
    :delete-table (update-in state [:tables] dissoc (:id value))
    :set-tables (assoc state :tables value :tables-loading false)
    :set-tables-loading (assoc state :tables-loading value)
    :set-function (assoc-in state [:functions (:id value)] value)
    :delete-function (update-in state [:functions] dissoc (:id value))
    :set-functions (assoc state :functions value :functions-loading false)
    :set-functions-loading (assoc state :functions-loading value)
    :open-editor-table (assoc state :table-editor {:meta value})
    :set-editor-table (assoc-in state [:table-editor :meta] value)
    :close-editor (assoc state :table-editor nil :function-editor nil)
    :set-editor-function (assoc state :function-editor {:function value})
    state))

(defn emit
  ([msg] (r/rswap! app-state shared-handler msg))
  ([msg handler] (js/console.log (clj->js @app-state)) (r/rswap! app-state handler msg)))

;; ----------------
;; Helpers

(defn function-outputs [function]
  (let [data (merge
               (get-in @app-state [:tables (:source function) :columns])
               (:operations function))]
    (map #(get data %) (:outputs function))))
