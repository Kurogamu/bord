(ns bord.state
  (:require
    [reagent.core :as r]))

;; ----------------
;; Model

(def default-state
  {:tables {}
   :table-editor nil
   :function-editor nil
   :table-uploader nil
   :tables-loading false
   :functions-loading false
   :tasks {:running {}
           :queued []
           :completed []
           :failed []}
   :task-viewer nil})

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
    :close-editor (assoc state :table-editor nil :function-editor nil :table-uploader nil)
    :set-editor-function (assoc state :function-editor {:function value})
    :set-table-upload-dialog (assoc state :table-uploader {})
    :show-tasks
    (if (nil? value)
      (assoc state :task-viewer nil)
      (assoc state :task-viewer {:state value}))
    state))

(defn emit
  ([msg] (r/rswap! app-state shared-handler msg))
  ([msg handler] (r/rswap! app-state handler msg)))

;; ----------------
;; Helpers

(defn function-outputs [function]
  (let [data (merge
               (get-in @app-state [:tables (:source function) :columns])
               (:operations function))]
    (map #(get data %) (:outputs function))))
