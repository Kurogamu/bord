(ns bord.file
  (:require
    [bord.data :refer [fetch-meta put-fragment]]
    [bord.fetch :refer [fetch-read-lines]]
    [clojure.string :as str]))

(defn- new-table [table-name columns preview]
  {:id (js/crypto.randomUUID)
   :name table-name
   :created (js/Date.now)
   :updated (js/Date.now)
   :columns (zipmap (map :id columns) columns)
   :sort-columns (map :id columns)
   :data-preview preview 
   :count 0})

(defn- new-column [column-name column-type]
  {:id (js/crypto.randomUUID)
   :name column-name
   :type column-type})

(defn- infer-column-type [data]
  (cond
    (re-matches #"[0-9., ]" data) :number
    (re-matches #"(true|false)" data) :boolean
    :else :string))

(defn- infer-columns [headers data-row]
  (letfn [(infered-column [header data]
            (new-column
              header
              (infer-column-type data)))]
    (mapv
      infered-column
      (map str/trim (str/split headers #","))
      (map str/trim (str/split data-row #",")))))

(defn infer-meta [file content]
  (let [columns (infer-columns (first content) (second content))
        preview-rows (take 5 (rest content))]
    (new-table
      (.-name file)
      columns
      (mapv
        #(zipmap (map :id columns) (str/split % #","))
        preview-rows))))

(defn create-fragment [{:as args :keys [object-id first-row data offset]}]
  (assoc args
         :id (js/crypto.randomUUID)
         :last-row (+ first-row (count data))))

(defn store-rows [data-rows table-meta row-counter fragment-counter on-progress]
  (if (nil? data-rows)
    (on-progress 1) ; update meta
    (let [processed
          (map
            #(zipmap (:sort-columns table-meta) (str/split % #","))
            (if (< 0 @row-counter) data-rows (rest data-rows)))
          fragment
          (create-fragment
            {:object-id (:id table-meta)
             :first-row @row-counter
             :offset @fragment-counter
             :data processed})]
      (swap! row-counter + (count processed))
      (swap! fragment-counter inc)
      (put-fragment
        {:data fragment
         :on-complete
         #(js/console.info "Pushed fragment " (clj->js fragment))
         :on-error
         #(js/console.error "Fragment failed " (clj->js fragment))}))))

(defn store-file-data [{:keys [url table-id on-progress]}]
  (let [row-counter (atom 0)
        fragment-counter (atom 0)]
    (fetch-meta
      {:table-id table-id
       :on-complete
       (fn [table-meta]
         (on-progress 0)
         (fetch-read-lines 
           url
           #(store-rows % table-meta row-counter fragment-counter on-progress)))
       :on-error #(js/console.error "fetch meta failed" %)})))
