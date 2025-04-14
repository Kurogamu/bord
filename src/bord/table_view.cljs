(ns bord.table-view
  (:require
    [reagent.core :as r]))

(defn column-header [{:keys [column]}]
  (if-let [column-name (:name column)]
    [:th column-name]
    [:th.blank "Blank"]))

(defn default-cell [{:keys [row-index column-id data]}]
  (if (clojure.string/blank? data)
    [:td.blank "Blank"]
    [:td (str data)]))

(defn table-row [{:keys [row-index row-data sort-columns cell-component]}]
  [:tr
   (map
     (fn [column-id]
       [cell-component {:key (str row-index column-id)
                        :row-index row-index
                        :column-id column-id
                        :data (get row-data column-id)}])
     sort-columns)
   ])

(defn table-component [{:keys [sort-columns columns data-rows cell-component]
                        :or {cell-component default-cell}}]
  [:div.table-wrapper
   [:table
    [:tr.header-row
     (map
       (fn [column-id]
         [column-header {:key column-id :column (get columns column-id)}])
       sort-columns)]
    (map-indexed
      (fn [index row-data]
        [table-row {:key index
                    :row-index index
                    :row-data row-data
                    :cell-component cell-component
                    :sort-columns sort-columns}])
      data-rows)]])
