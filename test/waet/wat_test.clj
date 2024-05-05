(ns waet.wat-test
  (:require [clojure.test :refer :all]
            [waet.values :as val]
            [waet.wat :refer [wat->wie]]))

(deftest wat->wie-test
  ;; Plural expressions.
  (are [wat wie] (= (wat->wie wat) wie)
    ""     '[]
    "   "  '[]

    "a b c"   '[a b c]
    "()()"    '[()()]

    ";"          '[]
    "; comment"  '[]
    "x(;;)y"     '[x y]
    "x(;;;)y"    '[x y]

    "; comment
    "            '[]
    "; comment
    abc"         '[abc]

    "x=1"      '[x= 1]
    "x=y"      '[x= y]
    "x=\"z\""  '[x= "z"]
    )
  ;; Singular expressions.
  (are [wat wie] (= (wat->wie wat) [wie])

    "abc"  'abc
    "x.y"  'x.y
    "x_y"  'x_y
    "$x"   '$x

    "(a (b c))"  '(a (b c))

    "123"   '123
    "-4"    '-4
    "+5"    '5
    "1.5"   '1.5
    "+1.5"  '1.5
    "-1.5"  '-1.5
    "1.0e3"    '1000.0
    "1.0e+3"   '1000.0
    "1.0e-3"   '0.001
    "+1.0e-3"  '0.001
    "-1.0e-3"  '-0.001
    "0x0a"     '10
    "-0x0a"    '-10
    "0x0.8"    '0.5
    "0x0.8p3"  (float 0x800)

    "\"\""     '""
    "\"x\""    '"x"
    "\"\\n\""  '"\n"

    "\"\\00\""              #waet/data[0]
    "\"\\u{0000}\""         "\0"
    "\"\\ff\\ff\\ff\\ff\""  #waet/data[255 255 255 255]
    "\"\\00\\01\""          #waet/data[0 1]
    "\"abc\\ff\""           #waet/data["abc" 0xFF]

    "(@x)"          (val/->Annotation 'x 'nil)
    "(@abc 1 2 3)"  (val/->Annotation 'abc '(1 2 3))

    ))

(deftest metadata-test
  (are [wat positions] (= (map #((juxt :line :column) (meta %))
                               (next (tree-seq sequential? seq (wat->wie wat))))
                          positions)
    "x"   [[1 1]]
    "xyz" [[1 1]]
    "  x" [[1 3]]
    "
x"        [[2 1]]
    "()"  [[1 1]]
    "(x)" [[1 1] [1 2]]
    "(  x
y)" [[1 1] [1 4] [2 1]]
    ))
