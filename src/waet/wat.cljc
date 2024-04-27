(ns waet.wat
  (:use [waet.util])
  (:require [instaparse.core :as insta]
            [waet.values :as val]))

(def grammar "

  file           =  expressions?
  <expression>   =  symbol | list | number | string | annotation
  <expressions>  =  <ws>* (expression (<ws> expression)*)? <ws>*

  (* Indirection puts metadata on tag because string before transform
     can't have metadata, but symbols can. *)
  symbol   =  -symbol
  -symbol  =  #'[a-zA-Z\\$][a-zA-Z0-9._]*'

  (* Indirection gets location information of the paren,
      instead of the first expression. That expression may not
      exist (be nil), and so cannot have metadata. *)
  list   =  -list
  -list  =  <'('> !#\";@\" expressions? <')'>

  annotation  =  <'(@'> symbol expressions? <')'>

  <number>  =  float | integer
  float     =  #'[0-9]+\\.[0-9]+'
  integer   =  #'[0-9]+'

  string         =  <'\"'> string-char* <'\"'>
  <string-char>  =  string-escape / #'[^\"\\\\]' (* TODO: Exclude < U+20 and U+7F *)

  <string-escape>     =  <'\\\\'> (string-escape-hex / string-escape-char)
  string-escape-hex   =  ('u{' hexnum '}')
  string-escape-char  =  #'[^u]'

  hexnum    =  hexdigit ('_'? hexnum)?
  hexdigit  =  #'[0-9a-fA-F]'

  ws     =  (space | comment)+
  space  =  #'\\s+'

  comment        =  line-comment | block-comment
  line-comment   =  #';[^\\n]*'
  block-comment  =  '(;' (#'[^;]*' | ';' !')')* ';)'

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
  args
  )

(defn decode-escape-char [c]
  (case c
    "0" "\0"
    "t" "\t"
    "n" "\n"
    "r" "\r"
    "\"" "\""
    "'" "'"
    (fail "unknown escape character" {:char c})))

(defn transform-annotation [head & tail]
  (val/->Annotation head tail))

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
   :annotation (metadata-transformer transform-annotation)
   })

(defn wat->wie [s]
  (->> (parser s)
       #?(:bb identity ; https://github.com/babashka/instaparse-bb/issues/7
          :clj (insta/add-line-and-column-info-to-metadata s))
       (insta/transform transformers)))

(comment

  (->
    ;"(@x)"
    (slurp "/Users/brandonbloom/Projects/wabt/test/decompile/code-metadata.txt")
    wat->wie
    first
    ;meta
    )

)
