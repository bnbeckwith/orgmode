;; ## Inline Element Formatting
;;
;; Aside from the block elements handled in the core, there are
;; particular elements withing text that have meaning. There are
;; roughly two types of elements: links and formatting.

(ns orgmode.inline
  (:require [clojure.string :as s]))

;; ### Link Elements

(def link-re #"\[\[([^\]]+)(?:\]\[([^\]]+))?\]\]")

(defn link-create
  "Create hyperlink structures from a list of regex matches"
  [coll]
  (for [[_ link text ] coll]
    {:type :link
     :uri link
     :content [(or text link)]}))

(def footnote-re #"\[(\d+|fn:(.*))\]")

(defn footnote-create
  "Creates footnote structures from a list of regex matches"
  [coll]
  (for [[_ ref name] coll]
    {:type :footnote-ref
     :id   (if (nil? name)
             ref name)}))

(def target-re #"<<\s*([^>]*?)\s*>>")

(defn target-create
  "Creates target structures from a list of regex matches"
  [tgts]
  (for [[_ lbl] tgts]
    {:type :target
     :id lbl}))

;; ### Timestamp Elements

;; hack to make it work with ranges
(def ts-base
  #"(\d{4})-(\d{2})-(\d{2}) (\w{3})(?: (\d{1,2}):(\d{2})(-(\d{1,2}):(\d{2})|.*)?)?")

(def ts-active-re
  (re-pattern (str "<" ts-base ">")))

(defn ts-active-create
  "Creates active timestamps from a list of regex matches"
  [coll]
  (for [[_ Y M D d h m _ Eh Em] coll]
    (cond-> {:type :timestamp
             :timetype :active
             :year Y
             :month M
             :day D
             :dayname d
             :hour h
             :minute m}
      (and Eh Em) (merge {:end-hour Eh :end-minute Em})
      :always or)))

(def ts-inactive-re
  (re-pattern (str "\\[" ts-base "\\]")))

(defn ts-inactive-create
  "Create inactive timestamps from a list of regex matches"
  [coll]
  (map #(assoc % :timetype :inactive)
       (ts-active-create coll)))

(def ts-range-re
  (re-pattern (str ts-active-re "--" ts-active-re)))

(defn ts-range-create
  "Create timestamp ranges from a list of regex matches"
  [coll]
  (for [[_ BY BM BD Bd Bh Bm _ _
         _ EY EM ED Ed Eh Em _ _] coll]
    {:type :timestamp
     :timetype :range
     :year    BY
     :month   BM
     :day     BD
     :dayname Bd
     :hour    Bh
     :minute  Bm
     :end-year    EY
     :end-month   EM
     :end-day     ED
     :end-dayname Ed
     :end-hour    Eh
     :end-minute  Em}))

;; ### Formatting Elements

(defn fmt-create
  "Generic function to create a format element from given type"
  [type]
  (fn [ts]
    (for [[_ t] ts]
      {:type type
       :content [t]})))

(defn fmt-re
  "Create a re-pattern to match the given delimiter s"
  [s]
  (re-pattern (str s #"(\S(?:.*?\S)??)" s)))

; ### Inline Processing

(defn re-interleave
  "Split l with on re, interleave this list with the inline elements
  constructed from ms using cfn"
  [re l cfn ms]
  (let [ls  (s/split l re)
        els (vec (map #(assoc % :inline true) (cfn ms)))]
    (if (empty? ls)
      els
      (vec
       (interleave ls (conj els {:inline true :type :comment :text "FIX INTERLEAVING"}))))))

(defn make-elem
  "Try to make any inline elements out of strings in coll using re and
  cstor to match and construct these elements"
  [coll re cstor]
  (flatten
   (for [elem coll]
     (if (string? elem)
       (if-let [ms (re-seq re elem)]
         (re-interleave re elem cstor ms)
         elem)
       elem))))

(defn parse-inline-elements
  "Takes line and breaks it into inline elements and interleaving
  text"
  [line]
  (-> [line]
      (make-elem link-re        link-create)
      (make-elem footnote-re    footnote-create)
      (make-elem target-re      target-create)
      (make-elem ts-range-re    ts-range-create)
      (make-elem ts-active-re   ts-active-create)
      (make-elem ts-inactive-re ts-inactive-create)
      (make-elem (fmt-re "\\*") (fmt-create :bold))
      (make-elem (fmt-re "/")   (fmt-create :italic))
      (make-elem (fmt-re "\\+") (fmt-create :strike-through))
      (make-elem (fmt-re "_")   (fmt-create :underline))
      (make-elem (fmt-re "=")   (fmt-create :verbatim))
      (make-elem (fmt-re "~")   (fmt-create :code))))
