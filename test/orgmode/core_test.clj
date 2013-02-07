(ns orgmode.core-test
  (:use clojure.test
        [orgmode.core :as core]))


(def o 
"#+TAGS: Sample

* Headline 1

  Body text for headline one with one *bolded item*

** Tasks, or things TODO
:PROPERTIES:
:foo: bar
:foos: baz baz baz
:END:

*** TODO This needs done

*** DONE This is a completed task

#+begin_src clojure
(def foo :bar)
#+end_src

")

(def root (core/zip (core/parse-str o)))

;; TODO -- add in deadlines, scheduled, etc.
(deftest headlines
  (testing "Headlines"
    (is (= 1 (count (core/getall 
                     root 
                     #(and (= 1 (:level %))
                           (= :headline (:type %)))))))
    (is (= 2 (count (core/getall
                     root
                     (comp not nil? :todo)))))))


(deftest elements
  (testing "Elements"
    (is (= "bolded item"
           ((comp :content first)
            (core/getall 
             root
             #(= :bold (:type %))))))))