(ns waet.encode-test
  (:require [clojure.test :refer :all]
            [waet.encode :refer :all]))

(deftest leb128-test
  (are [n bs] (= (map #(bit-and % 0xFF) (encode-signed-leb128 n)) bs)
    0 [0]
    1 [1]
    624485 [0xE5 0x8E 0x26]
    )
  (are [n bs] (= (map #(bit-and % 0xFF) (encode-unsigned-leb128 n)) bs)
    0 [0]
    1 [1]
    624485 [0xE5 0x8E 0x26]
    ))
