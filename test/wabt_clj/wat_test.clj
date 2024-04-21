(ns wabt-clj.wat-test
  (:require [clojure.test :refer :all]
            [wabt-clj.wat :refer [wat->wie]]))

(deftest wat->edn
  (are [wat wie] (= (wat->wie wat) wie)
    "" '[]
    "   " '[]
    "abc" '[abc]
    "x.y" '[x.y]
    "a b c" '[a b c]
    "(a (b c))" '[(a (b c))]
    ";" '[]
    "; comment" '[]

    "; comment
    " []
    "; comment
    abc" '[abc]

    "123" '[123]
    "1.5" '[1.5]

    ))
