(ns bord.worker
  (:require
    [bord.file :refer [store-file-data]]
    [bord.data :refer [db-init]]
    [clojure.edn :refer [read-string]]))

(defn post-message [msg]
  (js/postMessage (pr-str msg)))

(defn store-data [[url table-id]]
  (letfn [(on-progress [progress]
            (post-message [:table-loading-progress [table-id progress]]))]
    (post-message [:add-loading-table table-id])
    (store-file-data {:url url
                      :table-id table-id
                      :on-progress on-progress})))

(defn message-handler [e]
  (let [[event value] (read-string (.-data e))]
    (case event
      :store (store-data value)
      (js/console.warning "unrecognized event " event))))

(defn init []
  (js/self.addEventListener "message" message-handler)
  (post-message [:worker-init "success"])
  (db-init {:on-success #(js/console.info "DB for worker loaded")
            :on-error #(js/console.error "DB for worker error " %)}))
