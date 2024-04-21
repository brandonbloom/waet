(ns wabt-clj.wat-test
  (:require [clojure.test :refer :all]
            [wabt-clj.wat :refer [wat->wie]]))

(deftest wat->edn
  (are [wat wie] (= (wat->wie wat) wie)
    "" '[]
    "   " '[]
    "abc" '[abc]
    "a b c" '[a b c]
    "(a (b c))" '[(a (b c))]
    ";" '[]
    "; comment" []
    "; comment
    " []

    ))
