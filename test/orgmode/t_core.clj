(ns orgmode.t-core
  (:use midje.sweet)
  (:require [orgmode.core :as core]))
        
;; Testing inlines


      
(def x (atom nil))

(defmacro test-inline "Perform a test on inline elements"
  [string content type]
  `(with-state-changes [(before :facts
                                (reset! x (core/parse-str ~string)))]
     (fact (get-in @x [:content 0 :content 0]) => ~content)
     (fact (get-in @x [:content 0 :type])      => ~type)
     (fact (get-in @x [:content 0 :inline])    => true)))

(test-inline "*Bold text*" "Bold text" :bold)
(test-inline "/Italic text/" "Italic text" :italic)
(test-inline "+Strike-through text+" "Strike-through text" :strike-through)
(test-inline "_Underline text_" "Underline text" :underline)
(test-inline "=Verbatim text=" "Verbatim text" :verbatim)
(test-inline "~Code text~" "Code text" :code)

