(ns orgmode.html
  (:require [hiccup.core :only [html]]
            [clojure.walk :as w]
            [clojure.java.shell :as s]
            [clojure.string :as t])
  (:import [org.apache.commons.lang3 StringEscapeUtils]))

(def img-suffix-re
  #"(?i)(png|bmp|jpg|jpeg)$")

(defn squish-seq [s]
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
       [:img {:src url :alt alt}]
       [:a {:href url} alt])))

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
  (let [type-map {"elisp" "cl"}
        c (s/sh "pygmentize" 
                "-f" "html" "-l" 
                (or (type-map (first (:attribs x)))
                    (first (:attribs x)))
           :in (t/join "\n" (:content x)))]
    (if (not (= (:exit c) 0))
      (list "<!-- pygmentize error: " (:err c) "\n Output" (:out c) "\n Error Code: " (:exit c) "\n-->"
       [:code
        [:pre 
         (StringEscapeUtils/escapeHtml4 (t/join "\n" (:content x)))]])
      (:out c))))
  
(defn make-para [coll]
  (let [ps (partition-by #(or nil? (re-matches #"\s+" %)) coll)]
    (map (fn [y] (into [:p] (map hiccupify y))) ps)))

(defmethod blockprocess :default [x]
  (into [(:block x)] (make-para (:content x))))

(defmethod hiccupify :headline [x]
  (if (and (:tags x) ((:tags x) "noexport"))
    ""
    (list 
     [(keyword (str "h" (inc (:level x))))
       (:text x)]
     (hiccupify (:content x)))))

(defmethod hiccupify :root [x]
  (into [:div] (hiccupify (:content x))))

(defmethod hiccupify :list [x]
  (into [(:listtype x)] (hiccupify (:content x))))

(defmethod hiccupify :listitem [x]
  (into (into [:li] (map hiccupify (:text x))) (hiccupify (:content x))))

(defmethod hiccupify :table [x]
  (let [rows (:rows x)
        hdr? (= (second rows) :tline)
        tbl  [:table
              (when hdr?
                (into [:tr] (map (fn [x] [:th (hiccupify x)]) (first rows))))]
        rows (if hdr? (next (filter #(not (= :tline %)) rows)) rows)]
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
  [:verbatim (:content x)])

(defmethod hiccupify :target [x]
  [:a {:id (:id x) :href (str "#" (:id x))}])

(defmethod hiccupify :seq [x]
  (letfn [(p-able [s]
            (or (and (string? s)
                     (not (empty? s)))
                (:inline s)))]
    (for [s (partition-by p-able x)]
      (if (p-able (first s))
        [:p (map hiccupify s)]
        (map hiccupify s)))))

(defmethod hiccupify :default [x]
  (str x "\n"))
