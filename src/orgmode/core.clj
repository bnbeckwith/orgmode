;; ## Core functionality
;;
;; These are the main parsing functions

(ns orgmode.core
  (:require [clojure.string :as s]
            [clojure.zip :as zip]
            [clojure.java.io :as io]
            [orgmode.block :as block]
            [orgmode.html :as html]))

;; ### Parsing functions
;;
;; These two are the workhorses that will parse an orgmode file, or
;; sequence of lines and produce a tree structures representing the
;; parsed elements.

(defn parse-lines
  "Parse coll as a sequence of orgmode lines"
  [coll]
  (block/parse-lines coll))

(defn parse-str
  "Split a string and parse it"
  [x]
  (parse-lines (s/split-lines x)))

(defn parse
  "Open x and parse lines"
  [x]
  (with-open [r (io/reader x)]
    (parse-lines (line-seq r))))

;; ### Generated formatted text
(defn convert
  "Convert the structure to html

  The user can supply a function for handling source blocks."
  ([r]
   (html/org-to-html r))
  ([r f]
   (binding [html/*user-src-fn* f]
     (convert r))))


;; ### Utilities
;;
;; The functions aid in the handling of the resulting tree from the
;; parsing functions.

(defn zip
  "Returns a zipper from org elements (from orgmode/parse)"
  [root]
  (zip/xml-zip root))

(defn getall
  "Returns all nodes from root satisifying f"
  [root f]
  (loop [acc [], z root]
    (let [acc (if (f (zip/node z))
                (conj acc (zip/node z))
                acc)]
      (if (zip/end? z)
        acc
        (recur acc (zip/next z))))))
