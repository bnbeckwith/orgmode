(ns orgmode.t-core
  (:use midje.sweet)
  (:require [orgmode.core :as core]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;        
;; Testing inlines
 
(def x (atom nil))

(defmacro test-inline-fmt "Perform a test on inline elements"
  [string content type]
  `(with-state-changes [(before :facts
                                (reset! x (core/parse-str ~string)))]
     (fact (get-in @x [:content 0 :content 0]) => ~content)
     (fact (get-in @x [:content 0 :type])      => ~type)
     (fact (get-in @x [:content 0 :inline])    => true)))


(defmacro test-inline "Check inline elment parsing"
  [string & path-result-pairs]
  (when path-result-pairs
    (let [fs (for [[p r] (partition 2 path-result-pairs)]
               `(fact (get-in @x ~p) => ~r))]
      `(fact ~string
             (with-state-changes [(before :facts
                                          (reset! x (core/parse-str ~string)))]
               ~@fs)))))

;; Formatting
(fact "Formatting"
      (test-inline-fmt  "*Bold text*"      "Bold text"      :bold)
      (test-inline-fmt  "/Italic text/"    "Italic text"    :italic)
      (test-inline-fmt  "+Striken text+"   "Striken text"   :strike-through)
      (test-inline-fmt  "_Underline text_" "Underline text" :underline)
      (test-inline-fmt  "=Verbatim text="  "Verbatim text"  :verbatim)
      (test-inline-fmt  "~Code text~"      "Code text"      :code))
      
;; Footnotes
(fact "Footnotes"
      (test-inline "Footnote [1]"
                   [:content 1 :type   ] :footnote-ref
                   [:content 1 :id     ] "1"
                   [:content 1 :inline ] true)
      
      (test-inline "Foornote [fn:blue]"
                   [:content 1 :type   ] :footnote-ref
                   [:content 1 :id     ] "blue"
                   [:content 1 :inline ] true))

;; Links
(fact "Links"
      (test-inline "[[http://bnbeckwith.com]]"
                   [:content 0 :uri       ] "http://bnbeckwith.com"
                   [:content 0 :type      ] :link
                   [:content 0 :inline    ] true
                   [:content 0 :content 0 ] "http://bnbeckwith.com")
      
      (test-inline "[[http://bnbeckwith.com][website]]"
                   [:content 0 :uri       ] "http://bnbeckwith.com"
                   [:content 0 :type      ] :link
                   [:content 0 :inline    ] true
                   [:content 0 :content 0 ] "website"))

;; Target
(fact "Targets"
      (test-inline "<<target>>"
                   [:content 0 :id     ] "target"
                   [:content 0 :inline ] true
                   [:content 0 :type   ] :target)

      (test-inline "<<target two>>"
                   [:content 0 :id     ] "target two"
                   [:content 0 :inline ] true
                   [:content 0 :type   ] :target))

;; Active Timestamp
(fact "Active Timestamps"
      (test-inline "<1900-01-01 Mon>"
                   [:content 0 :inline   ] true
                   [:content 0 :type     ] :timestamp             
                   [:content 0 :timetype ] :active
                   [:content 0 :year     ] "1900"
                   [:content 0 :month    ] "01"
                   [:content 0 :day      ] "01"
                   [:content 0 :dayname  ] "Mon"
                   [:content 0 :hour     ] nil
                   [:content 0 :minute   ] nil)
      
      (test-inline "<1900-01-01 Mon 12:12>"
                   [:content 0 :inline   ] true
                   [:content 0 :type     ] :timestamp             
                   [:content 0 :timetype ] :active
                   [:content 0 :year     ] "1900"
                   [:content 0 :month    ] "01"
                   [:content 0 :day      ] "01"
                   [:content 0 :dayname  ] "Mon"
                   [:content 0 :hour     ] nil
                   [:content 0 :minute   ] nil)
      
      (test-inline "<1900-01-01 Mon 12:12-1:30>"
                   [:content 0 :inline   ] true
                   [:content 0 :type     ] :timestamp             
                   [:content 0 :timetype ] :active
                   [:content 0 :year     ] "1900"
                   [:content 0 :month    ] "01"
                   [:content 0 :day      ] "01"
                   [:content 0 :dayname  ] "Mon"
                   [:content 0 :hour     ] nil
                   [:content 0 :minute   ] nil))
      
;; Inactive Timestamp
(fact "Inactive Timestamps"
      (test-inline "[1900-01-01 Mon]"
                   [:content 0 :inline   ] true
                   [:content 0 :type     ] :timestamp             
                   [:content 0 :timetype ] :inactive
                   [:content 0 :year     ] "1900"
                   [:content 0 :month    ] "01"
                   [:content 0 :day      ] "01"
                   [:content 0 :dayname  ] "Mon"
                   [:content 0 :hour     ] nil
                   [:content 0 :minute   ] nil)
      
      (test-inline "[1900-01-01 Mon 12:12]"
                   [:content 0 :inline   ] true
                   [:content 0 :type     ] :timestamp             
                   [:content 0 :timetype ] :active
                   [:content 0 :year     ] "1900"
                   [:content 0 :month    ] "01"
                   [:content 0 :day      ] "01"
                   [:content 0 :dayname  ] "Mon"
                   [:content 0 :hour     ] "12"
                   [:content 0 :minute   ] "12")
      
      (test-inline "[1900-01-01 Mon 12:12-1:30]"
                   [:content 0 :inline     ] true
                   [:content 0 :type       ] :timestamp             
                   [:content 0 :timetype   ] :active
                   [:content 0 :year       ] "1900"
                   [:content 0 :month      ] "01"
                   [:content 0 :day        ] "01"
                   [:content 0 :dayname    ] "Mon"
                   [:content 0 :hour       ] "12"
                   [:content 0 :minute     ] "12"
                   [:content 0 :end-hour   ] "1"
                   [:content 0 :end-minute ] "30"))


;; Range
(fact "Timestamp Ranges"
      (test-inline "<1900-01-01 Mon>--<1901-01-01 Tue>"
                   [:content 0 :inline      ] true
                   [:content 0 :type        ] :timestamp             
                   [:content 0 :timetype    ] :active
                   [:content 0 :year        ] "1900"
                   [:content 0 :month       ] "01"
                   [:content 0 :day         ] "01"
                   [:content 0 :dayname     ] "Mon"
                   [:content 0 :hour        ] nil
                   [:content 0 :minute      ] nil
                   [:content 0 :end-year    ] "1900"
                   [:content 0 :end-month   ] "01"
                   [:content 0 :end-day     ] "01"
                   [:content 0 :end-dayname ] "Mon"
                   [:content 0 :end-hour    ] nil
                   [:content 0 :end-minute  ] nil)


      (test-inline "<1900-01-01 Mon 2:55>--<1901-01-01 Tue 6:40>"
                   [:content 0 :inline      ] true
                   [:content 0 :type        ] :timestamp             
                   [:content 0 :timetype    ] :active
                   [:content 0 :year        ] "1900"
                   [:content 0 :month       ] "01"
                   [:content 0 :day         ] "01"
                   [:content 0 :dayname     ] "Mon"
                   [:content 0 :hour        ] "2"
                   [:content 0 :minute      ] "55"
                   [:content 0 :end-year    ] "1900"
                   [:content 0 :end-month   ] "01"
                   [:content 0 :end-day     ] "01"
                   [:content 0 :end-dayname ] "Mon"
                   [:content 0 :end-hour    ] "6"
                   [:content 0 :end-minute  ] "40"))

