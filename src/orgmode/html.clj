;; ## HTML output
;;
;; Functions to facilitate HTML output

(ns orgmode.html
  (:require [hiccup.core :only [html]]
            [clojure.walk :as w]
            [clojure.string :as t])
  (:import [org.apache.commons.lang3 StringEscapeUtils]))


;; ### Helping Items

(def img-suffix-re
  #"(?i)(png|bmp|jpg|jpeg)$")

(def ^:dynamic *user-src-fn*
  "User-defined function to use for SRC blocks of code. This
  function will be called with the SRC block map passed in."
  (fn [x] nil))

(defn squish-seq
  "For any consecutive sequences, merge them into one."
  [s]
  (letfn [(concat-seq ([] [])
            ([x y]
               (vec
                (if (seq? y)
                  (concat x (squish-seq y))
                  (into x [y])))))]
    (reduce concat-seq [] s)))

(defn maybe-img
  ([url] (maybe-img url url))
  ([url alt]
     (if (re-find img-suffix-re url)
       [:figure [:img {:src url :alt alt}] (into [:figcaption] alt)]
       (into [:a {:href url}] alt))))

(defn elmtype [x]
  (if (map? x)
    (if-let [t (:type x)]
      t
      :root)
    (if (sequential? x)
      :seq
      :default)))

(defmulti hiccupify elmtype)
(defmulti blockprocess :block)

(defn hiccup [r]
  (squish-seq (hiccupify r)))

(defn org-to-html [r]
  (hiccup.core/html (hiccupify r)))

(defmethod blockprocess :src [x]
  (if-let [c (and *user-src-fn* (*user-src-fn* x))]
    c
    [:pre
     [:code
      (StringEscapeUtils/escapeHtml4 (t/join "\n" (:content x)))]]))

(defmethod blockprocess :default [x]
  (into [(:block x)] (:content x)))

(defmethod hiccupify :headline [x]
  (if (and (:tags x) ((:tags x) "noexport"))
    ""
    [:section {:class (str "hsec" (inc (:level x)) (:tags x))}
     [(keyword (str "h" (inc (:level x))))
      (:text x)]
     (hiccupify (:content x))]))

(defmethod hiccupify :root [x]
  (into [:section {:id "root" } ] (hiccupify (:content x))))

(defmethod hiccupify :list [x]
  (into [(:listtype x)] (hiccupify (:content x))))

(defmethod hiccupify :listitem [x]
  (into [:li] (map hiccupify (:content x))))

(defmethod hiccupify :table [x]
  (let [rows (:rows x)
        hdr? (= (second rows) :tline)
        tbl  [:table
              (when hdr?
                (into [:tr] (map (fn [x] [:th (hiccupify x)]) (first rows))))]
        rows (if hdr? (next (filter #(not= :tline %) rows)) rows)]
    (into tbl
          (for [row rows]
            (into [:tr]
                  (map (fn [x] [:td (hiccupify x)]) row))))))

(defmethod hiccupify :block [x]
  (blockprocess x))

(defmethod hiccupify :link [x]
  (let [{:keys [uri content]} x]
     (maybe-img uri content)))

;; TODO -- consider using :cite?
(defmethod hiccupify :footnote-ref [x]
  [:a.footnote {:href (str "#" (:id x))}
   (:id x)])

(defmethod hiccupify :footnote [x]
  (into [:a.footnote-def
         {:id (:id x)
          :href (str "#" (:id x))}]
   (hiccupify (:content x))))

;; TODO -- figure out timestamps

(defmethod hiccupify :bold [x]
  (into [:strong] (hiccupify (:content x))))

(defmethod hiccupify :italic [x]
  (into [:em] (hiccupify (:content x))))

(defmethod hiccupify :underline [x]
  (into [:u] (hiccupify (:content x))))

(defmethod hiccupify :code [x]
  (into [:code] (hiccupify (:content x))))

(defmethod hiccupify :comment [x]
  (list "<!-- "
        (hiccupify (:content x))
        " -->"
        (when-let [tgts (filter #(= :target (:type %)) (:content x))]
          (map hiccupify tgts))))

(defmethod hiccupify :verbatim [x]
  (into [:verbatim] (:content x)))

(defmethod hiccupify :target [x]
  [:a {:id (:id x) :href (str "#" (:id x))}])

(defmethod hiccupify :p [x]
   [:p (hiccupify (:content x))])

(defmethod hiccupify :seq [x]
        (map hiccupify x))

(defmethod hiccupify :default [x]
  (str x "\n"))
