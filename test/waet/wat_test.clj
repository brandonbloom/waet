(ns waet.wat-test
  (:require [clojure.test :refer :all]
            [waet.values :as val]
            [waet.wat :refer [wat->wie]]))

(deftest wat->wie-test
  (are [wat wie] (= (wat->wie wat) wie)
    ""     '[]
    "   "  '[]

    "a b c"      '[a b c]

    ";"          '[]
    "; comment"  '[]
    "x(;;)y"     '[x y]
    "x(;;;)y"    '[x y]

    "; comment
    "            '[]
    "; comment
    abc"         '[abc]
    )
  (are [wat wie] (= (wat->wie wat) [wie])

    "abc"  'abc
    "x.y"  'x.y
    "x_y"  'x_y
    "$x"   '$x

    "(a (b c))"  '(a (b c))

    "123"  '123
    "1.5"  '1.5

    "\"\""     '""
    "\"x\""    '"x"
    "\"\\0\""  '"\0"
    "\"\\n\""  '"\n"

    "x=1"      '{x 1}
    "x=y"      '{x y}
    "x=\"z\""  '{x "z"}

    "(@x)"          (val/->Annotation 'x 'nil)
    "(@abc 1 2 3)"  (val/->Annotation 'abc '(1 2 3))

    ))

(deftest metadata-test
  (are [wat line column] (= (-> wat wat->wie first meta (select-keys [:line :column]))
                            {:line line :column column})
    "x"   1 1
    "xyz" 1 1
    "  x" 1 3
    "
x"        2 1
    "()"  1 1
    "(x)" 1 1
    ))
