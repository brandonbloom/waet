(ns waet.wat
  (:use [waet.util])
  (:require [instaparse.core :as insta]))

(def grammar "
  file = expressions?
  <expression> = symbol | list | number | string
  <expressions> = <ws>* (expression (<ws> expression)*)? <ws>*
  symbol = -symbol                       (* Indirection puts metadata on tag. *)
  -symbol = #'[a-zA-Z\\$][a-zA-Z0-9._]*'   (* Strings can't have metadata. *)
  list = -list                           (* Tag gives location of paren. *)
  -list = <'('> !';' expressions? <')'>  (* Expressions can be nil; no metadata. *)
  <number> = float | integer
  float = #'[0-9]+\\.[0-9]+'
  integer = #'[0-9]+'
  string = <'\"'> string-char* <'\"'>
  <string-char> = string-escape / #'[^\"\\\\]' (* TODO: Exclude < U+20 and U+7F *)
  <string-escape> = <'\\\\'> (string-escape-hex / string-escape-char)
  string-escape-hex = ('u{' hexnum '}')
  string-escape-char = #'[^u]'
  hexnum = hexdigit ('_'? hexnum)?
  hexdigit = #'[0-9a-fA-F]'
  ws = (space | comment)+
  space = #'\\s+'
  comment = line-comment | block-comment
  line-comment = #';[^\\n]*'
  block-comment = '(;' (#'[^;]*' | ';' !')')* ';)'
")

(def parser (insta/parser grammar))

(defn munged-symbol [s]
  ;; TODO: Escape/replace problematic characters in s.
  (symbol s))

(defn metadata-transformer [f]
  (fn [& xs]
    (let [m (or (meta xs) (some meta xs))
          {:instaparse.gll/keys [start-line start-column]} m
          y (apply f xs)]
      (if (and start-line start-column)
        (with-meta y {:line start-line :column start-column})
        y))))

(defn decode-escape-hex [& args]
  (prn 'hex args)
  args
  )

(defn decode-escape-char [c]
  (case c
    "t" "\t"
    "n" "\n"
    "r" "\r"
    "\"" "\""
    "'" "'"))

(def transformers
  {:file (metadata-transformer vector)
   :list (metadata-transformer identity)
   :-list list
   :symbol (metadata-transformer identity)
   :-symbol munged-symbol
   :float #(Double/parseDouble %)
   :integer #(Long/parseLong %)
   :string str
   :string-escape-hex decode-escape-hex
   :string-escape-char decode-escape-char
   })

(defn wat->wie [s]
  (->> (parser s)
       #?(:bb identity ; https://github.com/babashka/instaparse-bb/issues/7
          :clj (insta/add-line-and-column-info-to-metadata s))
       (insta/transform transformers)))

(comment

(->
  (wat->wie "$x")
  first
  meta
  )

)
