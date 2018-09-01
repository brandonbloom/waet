(ns wasm-clj.analyze3
  (:require [wasm-clj.parse :as parse])
  (:use [wasm-clj.util]))

;;; Module Fields.

(defmulti -modulefield :head)

(defn modulefield [form]
  (-modulefield (parse/modulefield form)))

(defmethod -modulefield 'type [ast]
  (fail "not analyze/-modulefield 'type"))

(defmethod -modulefield 'import [ast]
  [ast]) ;XXX (fail "not analyze/-modulefield 'import")

(defmethod -modulefield 'func [ast]
  (if-let [{:keys [module name]} (:import ast)]
    (modulefield (list 'import module name
                       (concat ['func]
                               (when-let [id (:id ast)]
                                 [id])
                               (-> ast :type :forms))))
    (if-let [{:keys [name tail]} (:export ast)]
      (let [id (gensym name)]
        (concat (modulefield (list 'export name (list 'func id)))
                (modulefield (list* 'func id tail))))
      [ast])))

(defmethod -modulefield 'table [ast]
  (fail "not analyze/-modulefield 'table"))

(defmethod -modulefield 'mem [ast]
  (fail "not analyze/-modulefield 'mem"))

(defmethod -modulefield 'global [ast]
  (fail "not analyze/-modulefield 'global"))

(defmethod -modulefield 'export [ast]
  [ast]) ;XXX (fail "not analyze/-modulefield 'export")

(defmethod -modulefield 'start [ast]
  (fail "not analyze/-modulefield 'start"))

(defmethod -modulefield 'elem [ast]
  (fail "not analyze/-modulefield 'elem"))

(defmethod -modulefield 'data [ast]
  (fail "not analyze/-modulefield 'data"))

;;; Modules.

(defn module [form]
  (let [ast (parse/module form)
        ast (update ast :fields #(vec (mapcat modulefield %)))
        fields (group-by :head (:fields ast))
        funcs (fields 'func [])]
    (assoc ast
           :types (mapv :type funcs)
           :funcs funcs
           :imports (fields 'import [])
           :exports (fields 'export [])
           )))

;;; End.

(comment

  (module '(module))

  (modulefield '(func $i (import "imports" "imported_func") (param i32)))
  (modulefield '(func (export "exported_func") i32.const 42 call $i))

)
