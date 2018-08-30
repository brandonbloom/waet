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

(def no-match-exception (Exception. "no match"))

(defn no-match []
  ;; Uncomment exception allocation to get stacktraces for debugging.
  (throw (ex-info "no match" {::no-match true}))
  (throw no-match-exception))

(defn no-match? [ex]
  (or (identical? ex no-match-exception) ; Fast path (preallocated).
      (::no-match (ex-data ex))))        ; Slow path (with stacktrace).

(defmacro opt [& body]
  `(try
     ~@body
     (catch Exception ex#
       (when-not (no-match? ex#)
         (throw ex#)))))

(defn scan-pred [pred]
  (let [[x & xs] *input*]
    (if (pred x)
      (do (set! *input* xs)
          x)
      (no-match))))

(defn scan-all [scan]
  (loop [v []]
    (if-let [x (opt (scan))]
      (recur (conj v x))
      v)))

(defn scan-id []
  (scan-pred id?))

(defn scan-name []
  (scan-pred name?))

(defn scan-phrase [head]
  (scan-pred #(has-head? head %)))

(defn scan-import []
  (scan-phrase 'import))

(defn scan-typeuse []
  (let [type (opt (scan-phrase 'type))
        params (scan-all #(scan-phrase 'param))
        results (scan-all #(scan-phrase 'result))]
    {:type type
     :params params
     :results results
     :forms (concat (when type [type]) params results)}))

(defn scan-inline-export []
  (let [form (scan-phrase 'export)
        ast (s/conform (phrase-spec export :name name?) form)]
    (when (s/invalid? ast)
      (bad-syntax form "malformed export"))
    ast))

(defn scan-inline-import []
  (let [form (scan-phrase 'import)
        ast (s/conform (phrase-spec import :module name? :name name?)
                       form)]
    (when (s/invalid? ast)
      (bad-syntax form "malformed import"))
    ast))

(defn scan-locals []
  (scan-all #(scan-phrase 'local)))

(defn scan-importdesc []
  (let [form (scan-phrase 'func)] ;XXX func, table, memory, or global.
    form))

(defn scan-exportdesc []
  (let [form (scan-phrase 'func)] ;XXX func, table, memory, or global.
    form))

;;; Module Fields.

(defmulti -modulefield first)

(defn modulefield [form]
  (check-phrase form)
  (assoc (-modulefield form) :head (first form) :form form))

(defmethod -modulefield 'type [form]
  (fail "not implemented: parse/-modulefield 'type"))

(defmethod -modulefield 'import [[_ & tail]]
  (scanning tail
    (let [module (scan-name)
          name (scan-name)
          desc (scan-importdesc)]
      {:module module
       :name name
       :desc desc})))

(defmethod -modulefield 'func [[_ & tail]]
  (scanning tail
    (let [id (opt (scan-id))
          import (opt (scan-inline-import))]
      (if import
        (let [type (scan-typeuse)]
          {:id id
           :import import
           :type type})
        (let [export (opt (scan-inline-export))]
          (if export
            (let [tail (scan-rest)]
              {:id id
               :export export
               :tail tail})
            (let [type (scan-typeuse)
                  locals (scan-locals)
                  body (scan-rest)]
              {:id id
               :type type
               :locals locals
               :body body})))))))

(defmethod -modulefield 'table [form]
  (fail "not implemented: parse/-modulefield 'table"))

(defmethod -modulefield 'mem [form]
  (fail "not implemented: parse/-modulefield 'mem"))

(defmethod -modulefield 'global [form]
  (fail "not implemented: parse/-modulefield 'global"))

(defmethod -modulefield 'export [[_ & tail]]
  (scanning tail
    (let [name (scan-name)
          desc (scan-exportdesc)]
      {:name name
       :desc desc})))

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

  (fipp.edn/pprint
    (modulefield '(func $i (import "imports" "imported_func") (param i32))))
  (fipp.edn/pprint
    (modulefield '(func (export "exported_func") i32.const 42 call $i)))

  (party
    (module
      (func $i (import "imports" "imported_func") (param i32))
      ))

)
