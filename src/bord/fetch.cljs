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

(defn- decoding-reader [fetch-response]
  (-> (.-body fetch-response)
      (.pipeThrough (js/TextDecoderStream.))
      (.getReader)))

(defn- exec-read [reader callback]
  (-> (.read reader)
      (.then callback)
      (.catch #(js/console.error "failed to read" %))))

(defn- handle-read-result [read-result process-read-value]
  (if (.-done read-result)
    (do (process-read-value nil) false)
    (process-read-value (.-value read-result))))

(defn- read-loop [reader process-read-value]
  (exec-read
    reader
    (fn [read-result]
      (when (handle-read-result read-result process-read-value)
        (read-loop reader process-read-value)))))

(defn fetch-read [url process-read-value]
  (fetch
    url
    (fn [fetched]
      (read-loop (decoding-reader fetched) process-read-value))))

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
          (do
            (when-not (string/blank? @overflow)
              (read-callback (vector @overflow)))
            (read-callback nil))
          (read-callback (mid-lines chunk-value overflow)))))))
