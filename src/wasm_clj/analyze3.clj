(ns wasm-clj.analyze3
  (:require [wasm-clj.parse :as parse])
  (:use [wasm-clj.util]))

;;; Module Fields.

(defmulti -modulefield :head)

(defn modulefield [form]
  (-modulefield (parse/modulefield form)))

(defmethod -modulefield 'type [form]
  (fail "not analyze/-modulefield 'type"))

(defmethod -modulefield 'import [form]
  (fail "not analyze/-modulefield 'import"))

(defmethod -modulefield 'func [form]
  (fail "not analyze/-modulefield 'func"))

(defmethod -modulefield 'table [form]
  (fail "not analyze/-modulefield 'table"))

(defmethod -modulefield 'mem [form]
  (fail "not analyze/-modulefield 'mem"))

(defmethod -modulefield 'global [form]
  (fail "not analyze/-modulefield 'global"))

(defmethod -modulefield 'export [form]
  (fail "not analyze/-modulefield 'export"))

(defmethod -modulefield 'start [form]
  (fail "not analyze/-modulefield 'start"))

(defmethod -modulefield 'elem [form]
  (fail "not analyze/-modulefield 'elem"))

(defmethod -modulefield 'data [form]
  (fail "not analyze/-modulefield 'data"))

;;; Modules.

(defn module [form]
  (-> (parse/module form)
      (update :fields #(mapv modulefield %))))

;;; End.

(comment

  (module '(module))

  (modulefield '(func $i (import "imports" "imported_func") (param i32)))

)
