(ns waet.wat-test
  (:require [clojure.test :refer :all]
            [waet.wat :refer [wat->wie]]))

(deftest wat->wie-test
  (are [wat wie] (= (wat->wie wat) wie)
    "" '[]
    "   " '[]
    "abc" '[abc]
    "x.y" '[x.y]
    "x_y" '[x_y]
    "a b c" '[a b c]
    "(a (b c))" '[(a (b c))]
    ";" '[]
    "; comment" '[]
    "x(;;)y" '[x y]
    "x(;;;)y" '[x y]

    "; comment
    " []
    "; comment
    abc" '[abc]

    "123" '[123]
    "1.5" '[1.5]

    ))
