(ns bord.worker)


(defn init []
  (js/self.addEventListener
    "message"
    (fn [^js e]
      (js/console.log e)
      (js/postMessage #js {"echo-result" (.. e -data)} ))))
