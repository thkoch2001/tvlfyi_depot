(ns bbbg.meetup.import-test
  (:require [bbbg.meetup.import :as sut]
            [clojure.test :refer :all]))

(deftest test-row-user-id->user-id
  (is (= "246364067" (sut/row-user-id->user-id "user 246364067")))
  (is (= "246364067" (sut/row-user-id->user-id "246364067"))))
