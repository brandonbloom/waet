(ns waet.parse-test
  (:require [clojure.test :refer :all]
            [waet.values :as val]
            [waet.parse :as parse]))

(defn parse-v128 [input]
  (parse/scanning input
    (parse/scan-v128)))

(deftest parse-v128-test
  (parse-v128 '[i32x4 0 1 2 3])
  )
