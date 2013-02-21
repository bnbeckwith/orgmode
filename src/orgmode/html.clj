(ns orgmode.html
  (:require [hiccup.core :only [html]]
            [clojure.walk :as w]
            [clojure.java.shell :as s]
            [clojure.string :as t])
  (:import [org.apache.commons.lang3 StringEscapeUtils]))

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
    (map (fn [y] [:p (map hiccupify y)]) ps)))

(defmethod blockprocess :default [x]
  [(:block x) (make-para (:content x))])

(defmethod hiccupify :headline [x]
  (if (and (:tags x) ((:tags x) "noexport"))
    ""
    (list 
     [(keyword (str "h" (:level x)))
       (:text x)]
     (hiccupify (:content x)))))

(defmethod hiccupify :root [x]
  [:div (hiccupify (:content x))])

(defmethod hiccupify :list [x]
  [(:listtype x) (hiccupify (:content x)) ])

(defmethod hiccupify :listitem [x]
  [:li (hiccupify (:text x)) (hiccupify (:content x))])

(defmethod hiccupify :table [x]
  (into [:table]
        (for [row (:rows x)]
          (into [:tr] 
                (map (fn [x] [:td (hiccupify x)]) row)))))

(defmethod hiccupify :block [x]
  (blockprocess x))

(defmethod hiccupify :link [x]
  [:a {:href (:uri x)} (hiccupify (:content x))])

;; TODO -- consider using :cite?
(defmethod hiccupify :footnote-ref [x]
  [:a.footnote {:href (str "#" (:id x))}
   (:id x)])

(defmethod hiccupify :footnote [x]
  [:a.footnote-def 
   {:id (:id x)
    :href (str "#" (:id x))}
   (hiccupify (:content x))])

;; TODO -- figure out timestamps

(defmethod hiccupify :bold [x]
  [:strong (hiccupify (:content x))])

(defmethod hiccupify :italic [x]
  [:em (hiccupify (:content x))])

(defmethod hiccupify :underline [x]
  [:u (hiccupify (:content x))])

(defmethod hiccupify :code [x]
  [:code (hiccupify (:content x))])

(defmethod hiccupify :comment [x]
  (list "<!-- "
        (hiccupify (:content x))
        " -->"))

(defmethod hiccupify :verbatim [x]
  [:verbatim (:content x)])

(defmethod hiccupify :target [x]
  [:a {:id (:lbl x) :href (str "#" (:lbl x))}])

(defmethod hiccupify :seq [x]
  (map hiccupify x))

(defmethod hiccupify :default [x]
  (list x "\n"))