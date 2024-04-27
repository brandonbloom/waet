(ns waet.wat
  (:use [waet.util])
  (:require [instaparse.core :as insta]
            [waet.values :as val]))

(def grammar "

  file           =  expressions?
  <expression>   =  symbol | list | number | string | annotation
  <expressions>  =  <ws>* (-expressions (<ws> -expressions)*)? <ws>*
  <-expressions> =  expression | attribute

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

  <attribute>    =  attribute-key (symbol | number | string)
  attribute-key  =  symbol <'='>

  <number>  =  float | integer
  float     =  #'[0-9]+\\.[0-9]+'
  integer   =  #'[0-9]+'

  string         =  <'\"'> string-char* <'\"'>
  <string-char>  =  string-escape / #'[^\"\\\\]' (* TODO: Exclude < U+20 and U+7F *)

  <string-escape>          =  <'\\\\'> (string-escape-codepoint / string-escape-hex / string-escape-char)
  string-escape-codepoint  =  <'u{'> hexnum <'}'>
  string-escape-hex        =  hexdigit hexdigit
  string-escape-char       =  #'[^u0-9a-fA-F]'

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
  ;; TODO: Escape/replace problematic characters in s. -- Especially handle slash.
  (symbol s))

(defn metadata-transformer [f]
  (fn [& xs]
    (let [m (or (meta xs) (some meta xs))
          {:instaparse.gll/keys [start-line start-column]} m
          y (apply f xs)]
      (if (and start-line start-column)
        (with-meta y {:line start-line :column start-column})
        y))))

(defn transform-escape-char [c]
  (case c
    "t" "\t"
    "n" "\n"
    "r" "\r"
    "\"" "\""
    "'" "'"
    (fail "unknown escape character" {:char c})))

(defn transform-annotation [head & tail]
  (val/->Annotation head tail))

(defn transform-codepoint [n]
  (String. (-> n biginteger .toByteArray)
           java.nio.charset.StandardCharsets/UTF_8))

(defn transform-hexnum [& digits]
  (reduce (fn [acc d] (+ (* acc 16) d))
          (bigint 0)
          digits))

(defn transform-hexdigit [d]
  (let [i (-> d first int)]
    (cond
      (<= (int \0) i (int \9))  (- i (int \0))
      (<= (int \a) i (int \f))  (- i (int \a) -10)
      (<= (int \A) i (int \F))  (- i (int \A) -10)
      :else (fail "invalid hexdigit" {:digit d}))))

(def transformers
  {:file (metadata-transformer vector)

   :list (metadata-transformer identity)
   :-list list

   :symbol (metadata-transformer identity)
   :-symbol munged-symbol

   :float #(Double/parseDouble %)
   :integer #(Long/parseLong %)

   :string str
   :string-escape-codepoint transform-codepoint
   :string-escape-hex (comp char transform-hexnum)
   :string-escape-char transform-escape-char

   :annotation (metadata-transformer transform-annotation)

   :attribute-key (metadata-transformer #(munged-symbol (str % "=")))

   :hexnum transform-hexnum
   :hexdigit transform-hexdigit
   })

(defn wat->wie [s]
  (->> (parser s)
       #?(:bb identity ; https://github.com/babashka/instaparse-bb/issues/7
          :clj (insta/add-line-and-column-info-to-metadata s))
       (insta/transform transformers)))

(comment

  (->
    "\"\\u{0000}\""
    ;(slurp "/Users/brandonbloom/Projects/wabt/test/decompile/code-metadata.txt")
    wat->wie
    ;first
    ;meta
    )

)
