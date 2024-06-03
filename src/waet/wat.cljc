(ns waet.wat
  (:use [waet.util])
  (:require [clojure.java.io :as io]
            [instaparse.core :as insta]
            [waet.values :as val])
  (:import (java.lang Math)))

(def grammar
  (slurp (io/resource "waet/wat.ebnf")))

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
    "\\" "\\"
    "'" "'"
    (fail "unknown escape character" {:char c})))

(defn transform-annotation [head & tail]
  (val/->Annotation head tail))

(defn transform-codepoint [n]
  (String. (-> n biginteger .toByteArray)
           java.nio.charset.StandardCharsets/UTF_8))

(defn char->digit [d]
  (let [i (int d)]
    (cond
      (<= (int \0) i (int \9))  (- i (int \0))
      (<= (int \a) i (int \f))  (- i (int \a) -10)
      (<= (int \A) i (int \F))  (- i (int \A) -10)
      :else (fail "invalid digit" {:digit d}))))

(defn seq->digits [xs]
  (mapcat #(if (= % \_)
             []
             [(char->digit %)])
          xs))

(defn digits->bigint-base [base digits]
  (reduce (fn [acc d] (+ (* acc base) d))
          (bigint 0)
          (seq->digits digits)))

(defn digits->base [base digits]
  (let [n (digits->bigint-base base digits)]
    (if (<= Long/MIN_VALUE n Long/MAX_VALUE)
      (long n)
      n)))

(defn transform-hex-digits [digits]
  (digits->base 16 digits))

(defn transform-dec-digits [digits]
  (digits->base 10 digits))

(defn transform-data [& xs]
  (let [data (apply val/make-data xs)
        chunks (val/data-chunks data)]
    (case (count chunks)
      0 ""
      1 (if (-> chunks first string?)
          (first chunks)
          data)
      data)))

(defn transform-sign
  ([] 1)
  ([x]
   (case x
     ("+" "") 1
     "-" -1)))

(defn make-fractional [base n]
  (/ n (Math/pow base (count (str n)))))

(defn number-transformer [base]
  (fn f
    ([whole-sign whole-value]
     (* whole-sign whole-value))
    ([whole-sign whole-value fraction]
      (* whole-sign
         (+ whole-value (make-fractional base fraction))))
    ([whole-sign whole-value fraction exponent-sign exponent-value]
     (* (f whole-sign whole-value fraction)
        (Math/pow base (* exponent-sign exponent-value))))))

(def transformers
  {:file (metadata-transformer vector)

   :list (metadata-transformer identity)
   :-list list

   :symbol (metadata-transformer identity)
   :-symbol munged-symbol

   :dec-number (number-transformer 10)
   :hex-number (number-transformer 16)

   :dec-digits transform-dec-digits
   :hex-digits transform-hex-digits

   :sign transform-sign

   :string transform-data
   :string-escape-codepoint transform-codepoint
   :string-escape-hex transform-hex-digits
   :string-escape-char transform-escape-char

   :annotation (metadata-transformer transform-annotation)

   :attribute-key (metadata-transformer #(munged-symbol (str % "=")))
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
    "-0x0.8"
    ;(slurp "/Users/brandonbloom/Projects/wabt/test/decompile/code-metadata.txt")
    wat->wie
    ;first
    ;meta
    pprint
    )

)
