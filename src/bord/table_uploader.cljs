(ns bord.table-uploader
  (:require
    [bord.state :refer [app-state emit]]
    [bord.common :refer [animation-trigger]]
    [bord.worker-handler :refer [worker-emit]]
    [bord.data :refer [put-meta]]
    [bord.fetch :refer [fetch-read-lines]]
    [bord.file :refer [infer-meta]]
    [reagent.core :as r]
    [clojure.string :refer [blank?]]))

;; -------------------------
;; Model

(def modal-cursor (r/cursor app-state [:table-uploader]))

;; -------------------------
;; Update

(defn handler [state [event value]]
  (case event
    :init-closing (assoc-in state [:table-uploader :closing] true)
    :select-file (assoc-in state [:table-uploader :selected] value)
    :set-uploading (assoc-in state [:table-uploader :uploading] value)
    :set-data (assoc-in state [:table-uploader :result] value)
    :set-preview (assoc-in state [:table-uploader :preview] value)
    :set-meta (assoc-in state [:table-uploader :meta] value)
    state))

;; -------------------------
;; Task

(defn select-file [event]
  (let [file-object (-> event .-target .-files first)]
    (emit [:select-file file-object] handler)
    (fetch-read-lines
      (js/URL.createObjectURL file-object)
      #(emit [:set-meta (infer-meta file-object %)] handler))))

(defn start-upload []
  (let [url (->> (:selected @modal-cursor)
                 (.createObjectURL js/URL))
        table-meta (:meta @modal-cursor)]
    (emit [:set-uploading true] handler)
    (put-meta {:data table-meta
               :on-complete #(js/console.info "Data saved")
               :on-error #(js/console.error "Failed to create table!" %)})
    (worker-emit [:store [url (:id table-meta)]])))

(defn close-modal []
  (animation-trigger "slide-out" emit [:close-editor nil])
  (emit [:init-closing nil] handler))

;; -------------------------
;; View

(defn file-details [file]
  [:div.description
   [:div.description-field
    [:span.label "Filename"]
    [:span (.-name file)]]
   [:div.description-field
    [:span.label "Type"]
    [:span (.-type file)]]
   [:div.description-field
    [:span.label "Size"]
    [:span (.-size file)]]])

(defn column-description [[column-id column]]
  [:div
   {:key column-id
    :class "description-field"}
   [:span.label (:name column)]
   [:span
    (case (:type column)
      :number "Number"
      :boolean "Boolean"
      "String")]])

(defn column-section []
  [:div.modal-section
   [:h3 "Columns"]
   [:div.description
    (doall (map
             column-description
             (get-in @modal-cursor [:meta :columns])))]])

(defn preview-section [table-meta]
  [:div.modal-section
   [:h3 "Preview"]
   [:div.table-wrapper
    [:table
     [:tr
      (for [[column-id column] (:columns table-meta)]
        (if (blank? (:name column))
          [:th {:key column-id :class "blank"} "Blank"]
          [:th {:key column-id} (:name column)]))]
     (for [[row-index row-data] (map-indexed vector (:data-preview table-meta))]
       [:tr
        {:key row-index}
        (for [column-id (:sort-columns table-meta)]
          [:td
           {:key (str column-id row-index)}
           (str (get row-data column-id))])])]]])

(defn table-uploader []
  [:div
   {:class (if (:closing (:table-uploader @app-state))
             "modal modal-editor modal-editor-closing"
             "modal modal-editor")}
   [:div.modal-header
    [:div.modal-title "Upload Table"]
    [:div
     {:class "modal-menu btn-group"}
     [:button {:class "btn close-btn" :on-click close-modal} "Cancel"]]]
   [:div.modal-body
    [:div.modal-section
     [:h3 "Select File"]
     [:div.input-wrapper
      [:input
       {:type "file"
        :on-change select-file}]]
     (if-let [file (:selected @modal-cursor)]
       (file-details file))
     (if-let [result (:result @modal-cursor)]
       [:div.description result])]
    (if (:selected @modal-cursor)
      [column-section])
    (if (:meta @modal-cursor)
      (preview-section (:meta @modal-cursor)))
    [:div.modal-section
     [:h3 "Upload"]
     [:button
      {:on-click start-upload
       :disabled (not (:selected @modal-cursor))}
      "upload file"]]]])
