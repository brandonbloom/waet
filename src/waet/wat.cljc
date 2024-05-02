(ns waet.wat
  (:use [waet.util])
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta]
            [waet.values :as val]))

(def grammar
  (slurp (io/resource "wat.ebnf")))

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

  (defn inspect-ambiguity [s]
    (pprint (insta/parses parser s :unhide :all)))

  (inspect-ambiguity "()()")
  (inspect-ambiguity "(x (; y ;))")

  (->
    ;"x=1"
    "\"\\ff\\ff\\ff\\ff\\00\""
    ;(slurp "/Users/brandonbloom/Projects/wabt/test/decompile/code-metadata.txt")
    wat->wie
    ;first
    ;meta
    pprint
    )

)
