(ns bord.fetch
  (:require
    [clojure.string :as string]))

(defn fetch [url callback]
  (let [js-fetch (if (-> js/self .-document undefined?)
                   js/self.fetch
                   js/window.fetch)]
  (-> (js-fetch url)
    (.then callback)
    (.catch #(js/console.error "error fetching" %)))))

(defn- exec-read [reader callback]
  (-> (.read reader)
      (.then #(callback % reader))
      (.catch #(js/console.error "failed to read" %))))

(defn- process-chunk [read-result reader callback]
  (if (undefined? (.-value read-result))
    (callback nil)
    (when (callback (.-value read-result))
      (if (.-done read-result)
        (callback nil)
        (exec-read reader #(process-chunk %1 %2 callback))))))

(defn fetch-read [url chunk-callback]
  (fetch
    url
    (fn [fetch-response]
      (-> (.-body fetch-response)
          (.pipeThrough (js/TextDecoderStream.))
          (.getReader)
          (exec-read #(process-chunk %1 %2 chunk-callback))))))

(defn fetch-read-all [url read-callback]
  (let [result (atom "")]
    (fetch-read
      url
      (fn [chunk-value]
        (if (nil? chunk-value)
          (read-callback @result)
          (swap! result string/join chunk-value))))))

(defn- mid-lines [s overflow]
  (let [split-s (re-seq #"[^\r\n]+" s)
        prefixed (string/join @overflow (first split-s))
        end-line (string/ends-with? s "\n")]
    (reset! overflow (if end-line "" (last split-s)))
    (if end-line
      (cons prefixed (rest split-s))
      (drop-last (cons prefixed (rest split-s))))))

(defn fetch-read-lines [url read-callback]
  (let [overflow (atom "")]
    (fetch-read
      url
      (fn [chunk-value]
        (if (empty? chunk-value)
          (when-not (string/blank? @overflow)
            (read-callback (vector @overflow)))
          (read-callback (mid-lines chunk-value overflow)))))))
