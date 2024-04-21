(ns wabt-clj.wat
  (:use [wabt-clj.util])
  (:require [instaparse.core :as insta]))

(def grammar "
  file = expressions?
  <expression> = symbol | list | number
  <expressions> = <ws>* (expression (<ws> expression)*)? <ws>*
  symbol = #'[a-zA-Z][a-zA-Z0-9.]*'
  list = <'('> expressions? <')'>
  <number> = float | integer
  float = #'[0-9]+\\.[0-9]+'
  integer = #'[0-9]+'
  ws = (space | comment)
  space = #'\\s+'
  comment = #';[^\\n]*'
")

(def parser (insta/parser grammar))

(defn munged-symbol [s]
  ;; TODO: Escape/replace problematic characters in s.
  (symbol s))

(def transformers
  {:file vector
   :list list
   :symbol munged-symbol
   :float #(Double/parseDouble %)
   :integer #(Long/parseLong %)})

(defn wat->wie [s]
  (->> (parser s)
       (insta/transform transformers)))

(comment

  (wat->wie ";commnet
  ab.c")

)
