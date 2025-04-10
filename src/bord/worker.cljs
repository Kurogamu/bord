(ns bord.worker
  (:require
    [bord.file :refer [store-file-data]]
    [bord.data :refer [db-init fetch-function]]
    [bord.function-process :refer [process-fragments]]
    [clojure.edn :refer [read-string]]))

(defn post-message [msg]
  (js/postMessage (pr-str msg)))

(defn report-progress [progress]
  (post-message [:set-progress progress]))

(defn upload-data [[url table-id]]
  (store-file-data {:url url
                    :table-id table-id
                    :on-progress report-progress}))

(defn process-function [[function-id offset limit]]
  (process-fragments {:function-id function-id
                      :fragment-offset offset
                      :limit limit
                      :on-progress report-progress}))

(defn message-handler [e]
  (let [[event value] (read-string (.-data e))]
    (case event
      :upload-table (upload-data value)
      :process-function (process-function value)
      (js/console.warning "unrecognized event " event))))

(defn init-worker []
  (js/self.addEventListener "message" message-handler)
  (db-init {:on-success #(post-message [:worker-init "success"])
            :on-error #(js/console.error "DB for worker error " %)}))
