;; ## Block Element Formatting
;;
;; These functions parse an Org-mode file and generate the necessary
;; heirarchy of list and block elements.

(ns orgmode.block
  (:require [clojure.string :as s]
            [clojure.zip :as zip])
  (:use [orgmode.inline]))


;; ### Regular Expressions for Block Elements
;;
;; The following set of regular expressions match start and end
;; elements of blocks or block elements themselves.  Note that some
;; items are line items, but I considered them blocks of the
;; smallest size.

(def attrib-re
  "Attribute Regular Expression that captures the attribute name and
   any values (as a single string)"
  #"^#\+(\w+):\s*(.*)")

(def prop-open-re
  "Regex for the beginning of a properties block"
  #"\s*:PROPERTIES:\s*")

(def property-line-re
  "Regex that captures property keys and values from within a
   properties drawer"
  #"\s*:(\w+):\s*(.*)")

(def block-open-re
  "Regex that matches the start of a begin_* block. It captures the
   type of block along with any parameters (as a single string)"
  #"^#\+(?i)BEGIN_(\w+)(?:\s+(.*))?")

(def block-close-re
  "Regex that matches the end of a begin block"
  #"^#\+(?i)END_")

(def comment-re
  "Regex that matches an inline comment"
  #"^\s*#\s+(.*)")

(def headline-re
  "Regex that matches headlines and todo items. This regex does a
   bunch of work and captures the leading stars, TODO or DONE tags,
   headline text, and tags."
  ;; TODO -- capture priority here?
 #"^(\*+)\s+(?:(TODO|DONE)\s+)?(.*?)(?:\s+:(.*):)?")

(def plain-list-re
  "Regex that matches a plain list. This regex captues leading spaces,
   the bullets or indices, and item text"
  #"^(\s*)([-+*]|[0-9]+[.)])\s(.*)")

(def footnote-def-re
  "Regex denoting a footnote definition. Captures identifier and
   definition"
  #"^\s*\[(\d+|fn:(.*))\] (.*)")

(def table-re
  "Regex to match table lines. Captures fields as one string"
  #"^\s*\|(.*)")

(def table-formula-re
  "Regex to match table formula. Caputes the list of formulas as one
   string"
  #"\s*#\+TBLFM:\s*(.*)")

;; ### Processing Functions

(defmacro handle-last-line
  "Macro to help break out of further processing if line is
  nil. Returns root of z"
  [[line z] & body]
  `(if-not (nil? ~line)
     (do ~@body)
     (zip/root ~z)))

(declare next-line)

(defn parse-attrib
  "At current location z, add attribute with name and values"
  [[& rest] z [_ name values]]
  (fn []
    (next-line
     rest
     (zip/edit z
               #(assoc-in %1 [:attribs (keyword (.toLowerCase name))] values )))))

(defn parse-comment
  "At current location z, add comment with parsed text"
  [[& rest] z [_ comment]]
  (fn []
    (next-line
     rest
     (zip/append-child
      z
      {:type :comment
       :content (orgmode.inline/parse-inline-elements comment)}))))


(defn parse-footnote
  "Add current location z, add a footnote definition"
  [[& rest] z [_ nid fid text]]
  (fn []
    (next-line
     rest
     (zip/append-child
      z
      {:type :footnote
       :id (or fid nid)
       :content (orgmode.inline/parse-inline-elements text)}))))


(defn move-level
  "Given level, move up z until the location is an appropiate place to
  add elements at level."
  [z level]
  (let [cnode (zip/node z)
        clevel (get cnode :level level)]
  (if (> level clevel)
    z
    (recur (zip/up z) level))))

(defn parse-headline
  "Insert a headline at z and make it the new location."
  [[& rest] z [_ stars todo headline tag]]
  (let [lvl (count stars)
        zz (move-level z lvl)]
  #(next-line
    rest
    (-> zz
        (zip/append-child {:type :headline
                           :text headline
                           :todo todo
                           :level lvl
                           :content []
                           :tags (when tag
                                   (into #{}
                                         (s/split tag #":")))})
        zip/down
        zip/rightmost))))

(defn parse-property
  "Parse a property block, adding property keys and values until an
   end tag is encountered"
  ([[line & rest] z]
     {:pre [(= :headline (-> z zip/node (:type)))]}
     (handle-last-line
      [line z]
      (if-let [[_ prop value]
               (re-matches property-line-re line)]
        (if (= prop "END")
          #(next-line rest z)
          (fn [] (parse-property
                  rest
                  (zip/edit
                   z
                   #(assoc-in % [:properties (keyword (.toLowerCase prop))] value)))))
        (throw (Exception.
                (str "No :END: for propery block (looking at \""
                     line "\"" ))))))
  ([[& rest] z _] #(parse-property rest z)))

(defn parse-block
  "Parse a block structure, keeping attribues and adding lines until
   an end tag is encountered"
  ([[line & rest]  z]
     (handle-last-line
      [line z]
      (let [type (name (:block (zip/node z)))
            end-re (re-pattern (str block-close-re type #"\s*"))]
        (if (re-matches end-re line)
          #(next-line rest (zip/up z))
          #(parse-block rest (zip/append-child z line))))))
  ([[& rest] z [_ type attribs]]
     #(parse-block
       rest
       (-> z
           (zip/append-child {:type :block
                              :block (keyword (.toLowerCase type))
                              :content []
                              :attribs (when attribs
                                         (s/split attribs #"\s+"))})
           zip/down
           zip/rightmost))))


(defn enclosing-plain-list
  "Move z up to appropiate level enclosing the current list"
  [z level]
  (if (or (not= :list (:type (zip/node z)))
          (= level (:listlevel (zip/node z))))
    z
    (recur (zip/up z) level)))

(defn parse-plain-list
  "Parse a plain list, keeping track of any nested heirarchy."
  ([[line & rest :as lines] z [_ lvl idx text :as params]]
     (let [level (+ (count lvl) (count idx))
           listtype (if (re-find #"^[0-9]+" idx) :ol :ul)]
       (if (= :list (:type (zip/node z)))
         (if (= level (:listlevel (zip/node z)))
           (parse-plain-list
            lines
            (zip/append-child z
                              {:type :listitem
                               :content (orgmode.inline/parse-inline-elements text)}))
           (if (> level (:listlevel (zip/node z)))
             (parse-plain-list
              lines
              (-> z
                  (zip/down)
                  (zip/append-child {:type :list
                                     :listlevel level
                                     :listtype listtype
                                     :content [{:type :listitem
                                                :content (orgmode.inline/parse-inline-elements text)}]})
                  (zip/down)
                  (zip/rightmost)))
             (parse-plain-list
              lines
              (enclosing-plain-list z level)
              params)))
         (parse-plain-list
          lines
          (-> z
              (zip/append-child {:type :list
                                 :listlevel level
                                 :listtype listtype
                                 :content [{:type :listitem
                                            :content (orgmode.inline/parse-inline-elements text)}]})
              (zip/down)
              (zip/rightmost))))))
  ([[line & rest :as lines] z]
     (handle-last-line
      [line z]
      (let [indent-re (re-pattern
                       (format "(\\s{%d,})(.*)"
                               (:listlevel (zip/node z))))]
        (condp re-matches line
          plain-list-re :>> (partial parse-plain-list rest z)
          indent-re         :>> #(parse-plain-list
                                  rest
                                  (-> z
                                      (zip/down)
                                      (zip/append-child %1)
                                      (zip/up)))
          (next-line lines (zip/up z)))))))

(defn append-table-line
  "Append table row s to location z. Splits s into fields"
  [z s]
  (let [row (if (re-matches #"[-+]+\|?" s)
              :tline
              (map s/trim (s/split s #"\|")))]
    (zip/edit z
              #(update-in % [:rows] conj row))))

(defn add-table-formula
  "Add table formula at z, splitting s into separate formulas."
  [z s]
  (zip/edit z
       #(update-in % [:formulas] (s/split s #"::"))))

(defn parse-table
  "Parse a table to add at location z"
  [[& lines] z [_ tblline]]
  (let [fields (map s/trim (s/split tblline #"\|"))
        z' (-> z
               (zip/append-child {:type :table
                                  :content []
                                  :rows [fields]})
               (zip/down)
               (zip/rightmost))]
     (loop [ls lines
            z'' z']
       (handle-last-line
        [(first ls) z'']
        (if-let [[_ l] (re-matches table-re (first ls))]
          (recur
           (rest ls)
           (append-table-line z'' l))
          (if-let [[_ f] (re-matches table-formula-re
                                     (first ls))]
            (next-line
             (rest ls)
             (add-table-formula z'' f))
            (next-line ls (zip/up z''))))))))

(defn add-line-element
  "Parse line for inline elements and add sequence at z."
  [lines z line]
  #(next-line
    lines
    (if (re-matches #"\s*" line)
      (-> z
          (zip/append-child {:type :p
                             :content []})
          (zip/down)
          (zip/rightmost))

      (reduce
       zip/append-child
       z
       (orgmode.inline/parse-inline-elements line)))))

(defn next-line [[line & rest] z]
  "Process each line with the list of block element parsers. Return
   the root of the created zipper structure"
  (handle-last-line
   [line z]
   (condp re-matches line
     block-open-re   :>> (partial parse-block rest z)
     attrib-re       :>> (partial parse-attrib rest z)
     headline-re     :>> (partial parse-headline rest z)
     prop-open-re    :>> (partial parse-property rest z)
     plain-list-re   :>> (partial parse-plain-list rest z)
     table-re        :>> (partial parse-table rest z)
     footnote-def-re :>> (partial parse-footnote rest z)
     comment-re      :>> (partial parse-comment rest z)
     (add-line-element rest z line))))

(defn parse-lines
  "Parse coll as org-mode formatting"
  [coll]
  (let [root (zip/xml-zip {:level 0})]
    (trampoline next-line coll root)))
