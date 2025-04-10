(ns bord.worker
  (:require
    [bord.file :refer [store-file-data]]
    [bord.data :refer [db-init]]
    [clojure.edn :refer [read-string]]))

(defn post-message [msg]
  (js/postMessage (pr-str msg)))

(defn upload-data [[url table-id]]
  (letfn [(on-progress [progress]
            (post-message [:set-progress progress]))]
    (post-message [:add-loading-table table-id])
    (store-file-data {:url url
                      :table-id table-id
                      :on-progress on-progress})))

(defn message-handler [e]
  (let [[event value] (read-string (.-data e))]
    (case event
      :upload-table (upload-data value)
      (js/console.warning "unrecognized event " event))))

(defn init-worker []
  (js/self.addEventListener "message" message-handler)
  (db-init {:on-success #(post-message [:worker-init "success"])
            :on-error #(js/console.error "DB for worker error " %)}))
