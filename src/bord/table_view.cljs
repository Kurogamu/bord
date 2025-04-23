(ns bord.table-view
  (:require
    [reagent.core :as r]))

(defn column-header [{:keys [label]}]
  (if (clojure.string/blank? label)
    [:th.blank "Blank"]
    [:th label]))

(defn default-cell [{:keys [data]}]
  (if (clojure.string/blank? data)
    [:td.blank "Blank"]
    [:td (str data)]))

(defn table-row [{:keys [row-index
                         row-data
                         row-offset
                         sort-columns
                         cell-component]}]
  [:tr
   (when (some? row-offset)
     [default-cell {:key (str row-index "#")
                    :data (str row-index)}])
   (map
     (fn [column-id]
       [cell-component {:key (str row-index column-id)
                        :row-index (+ row-index row-offset)
                        :column-id column-id
                        :data (get row-data column-id)}])
     sort-columns)])

(defn table-component [{:keys [sort-columns
                               columns
                               data-rows
                               cell-component
                               row-offset]
                        :or {cell-component default-cell}}]
  [:div.table-wrapper
   [:table
    [:tr.header-row
     (cond->>
       (map
         (fn [column-id]
           [column-header
            {:key column-id
             :label (get-in columns [column-id :name])}])
         sort-columns)
       (some? row-offset) (cons [column-header {:key "index" :label "#"}]))]
    (map-indexed
      (fn [index row-data]
        [table-row {:key index
                    :row-index index
                    :row-offset row-offset
                    :row-data row-data
                    :cell-component cell-component
                    :sort-columns sort-columns}])
      data-rows)]])
