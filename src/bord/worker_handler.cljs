(ns bord.worker-handler)

(defonce worker (atom nil))

(defn- message-handler [e]
  (js/console.log "got message from worker " e))

(defn worker-emit [msg]
  (js/console.info "sending msg to worker" msg)
  (.postMessage @worker (pr-str msg)))

(defn init-worker []
  (reset! worker (js/Worker. "js/worker.js"))
  (.addEventListener @worker "message" message-handler))
