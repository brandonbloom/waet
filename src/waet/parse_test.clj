(ns waet.parse-test
  (:require [clojure.test :refer :all]
            [waet.values :as val]
            [waet.impl.scan :as scan]
            [waet.parse :as parse]))

(defn parse-v128 [input]
  (scan/from input
    (parse/scan-v128)))

(deftest parse-v128-test
  (are [input expected] (= (parse-v128 input) expected)
    '[i64x2 0 0]  0N
    '[i64x2 0 1]  1N
    '[i64x2 1 0]  (bigint (.shiftLeft (biginteger 1) 64))

    '[i32x4 0 1 2 3]  18446744082299486211N
    ))
