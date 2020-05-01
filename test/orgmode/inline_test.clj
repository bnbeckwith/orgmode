(ns orgmode.inline-test
  (:require [orgmode.inline :as sut]
            [midje.sweet :as midje]))

;; Active Timestamp
(midje/fact
 "Active TimeStamp Regex Test"

 (->> "1900-01-01 Mon 12:12" (re-seq sut/ts-base) first) =>
 ["1900-01-01 Mon 12:12" "1900" "01" "01" "Mon" "12" "12" "" nil nil]

 (->> "1900-01-01 Mon 12:00-13:15" (re-seq sut/ts-base) first ) =>
 ["1900-01-01 Mon 12:00-13:15" "1900" "01" "01" "Mon" "12" "00" "-13:15" "13" "15"]

 (->>  "1900-01-01 Mon 12:00" (re-seq sut/ts-base) sut/ts-active-create first) =>
 {:day "01",
  :dayname "Mon",
  :hour "12",
  :minute "00",
  :month "01",
  :timetype :active,
  :type :timestamp,
  :year "1900"}

 (->>  "1900-01-01 Mon 12:00-13:15" (re-seq sut/ts-base) sut/ts-active-create first) =>
 {:day "01",
  :dayname "Mon",
  :end-hour "13",
  :end-minute "15",
  :hour "12",
  :minute "00",
  :month "01",
  :timetype :active,
  :type :timestamp,
  :year "1900"})
