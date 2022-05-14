(ns orgmode.util
  (:require [clojure.zip :as zip]))

(defn fix-link [n]
  (merge n
         (when-let [ms (re-matches #"file:(.*)\.org" (:uri n))]
           {:uri (str (second ms) ".html")})))

(defn org-to-html-links [z]
  (if (zip/end? z)
    (zip/root z)
    (recur (zip/next
            (let [n (zip/node z)]
              (if (and (map? n)
                         (= :link (:type n)))
                (zip/edit z fix-link)
                z))))))
