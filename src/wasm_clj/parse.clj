(ns wasm-clj.parse
  (:require [clojure.spec.alpha :as s])
  (:use [wasm-clj.util]))

(defn bad-syntax [form message]
  (fail (str "syntax error: " message)
        {:form form}))

;;;; See https://webassembly.github.io/spec/core/text/index.html

;;; Values.

(defn name? [x]
  ;;TODO: Tighter validation.
  (string? x))

(defn id? [x]
  (and (simple-symbol? x)
       (= (first (str x)) \$)))

(defn value? [x]
  (or (int? x)
      (float? x)
      (string? x) ; Includes names.
      (id? x)))

(defn u32? [x]
  (and (int? x)
       (<= 0 x))) ;TODO: max.

(defn u64? [x]
  (and (int? x)
       (<= 0 x))) ;TODO: max.

;;; Utils.

(defmacro phrase-spec [head & body-specs]
  `(s/spec (s/and seq?
                  (s/cat :head #{'~head} ~@body-specs))))

(defn check-phrase [form]
  (when-not (seq? form)
    (bad-syntax form "expected list"))
  (when-not (simple-symbol? (first form))
    (bad-syntax form "expected symbol at head of list")))

(defn phrase? [form]
  (and (seq? form)
       (simple-symbol? (first form))))

(defn has-head? [head form]
  (and (phrase? form)
       (= (first form) head)))

;;; Scanning.

(def ^:dynamic *input*)

(defmacro scanning [input & body]
  `(binding [*input* ~input]
     (let [res# ~@body]
       (when-first [x# (seq *input*)]
         (bad-syntax x# "expected end of sequence"))
       res#)))

(defn scan-rest []
  (let [xs *input*]
    (set! *input* nil)
    (vec xs)))

(defn scan-pred [pred]
  (let [[x & xs] *input*]
    (when (pred x)
      (set! *input* xs)
      x)))

(defn scan-all [scan]
  (->> (repeatedly scan)
       (take-while some?)
       vec))

(defn scan-id []
  (scan-pred id?))

(defn scan-phrase [head]
  (scan-pred #(has-head? head %)))

(defn scan-import []
  (scan-phrase 'import))

(defn scan-typeuse []
  (let [type (scan-phrase 'type)
        params (scan-all #(scan-phrase 'param))
        results (scan-all #(scan-phrase 'result))]
    {:type type
     :params params
     :results results}))

(defn scan-export []
  (scan-phrase 'export))

(defn scan-import []
  (scan-phrase 'import))

(defn scan-locals []
  (scan-all #(scan-phrase 'local)))

;;; Module Fields.

(defmulti -modulefield first)

(defn modulefield [form]
  (check-phrase form)
  (-modulefield form))

(defmethod -modulefield 'type [form]
  (fail "not implemented: parse/-modulefield 'type"))

(defmethod -modulefield 'import [form]
  (fail "not implemented: parse/-modulefield 'import"))

(defmethod -modulefield 'func [[_ & tail :as form]]
  (scanning tail
    (let [id (scan-id)
          import (scan-import)]
      (if import
        (let [type (scan-typeuse)]
          {:head 'func
           :id id
           :import import
           :type type})
        (let [export (scan-export)
              type (scan-typeuse)
              locals (scan-locals)
              body (scan-rest)]
          {:head 'func
           :id id
           :export export
           :type type
           :locals locals
           :body body})))))

(defmethod -modulefield 'table [form]
  (fail "not implemented: parse/-modulefield 'table"))

(defmethod -modulefield 'mem [form]
  (fail "not implemented: parse/-modulefield 'mem"))

(defmethod -modulefield 'global [form]
  (fail "not implemented: parse/-modulefield 'global"))

(defmethod -modulefield 'export [form]
  (fail "not implemented: parse/-modulefield 'export"))

(defmethod -modulefield 'start [form]
  (fail "not implemented: parse/-modulefield 'start"))

(defmethod -modulefield 'elem [form]
  (fail "not implemented: parse/-modulefield 'elem"))

(defmethod -modulefield 'data [form]
  (fail "not implemented: parse/-modulefield 'data"))

;;; Modules.

(defn module [form]
  (let [data (s/conform (phrase-spec module :fields (s/* any?)) form)]
    (when (s/invalid? data)
      (bad-syntax form "expected module"))
    (-> data
        (update :fields #(or % [])))))

;;; End.

(comment

  (defmacro party [form]
    `(fipp.edn/pprint (module '~form)))

  ;;TODO Test more [MDN Examples](https://github.com/mdn/webassembly-examples).

  (module '(module))
  (module '(module 1 2 3))

  (modulefield '(func $i (import "imports" "imported_func") (param i32)))
  (modulefield '(func (export "exported_func") i32.const 42 call $i))

  (party
    (module
      (func $i (import "imports" "imported_func") (param i32))
      ))

)
