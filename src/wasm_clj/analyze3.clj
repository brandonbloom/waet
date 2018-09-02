(ns wasm-clj.analyze3
  (:require [wasm-clj.parse :as parse])
  (:use [wasm-clj.util]))

;;; Module Fields.

(def ^:dynamic *module*)

(defn fresh-id [name]
  (let [n (:counter *module*)]
    (change! *module* update :counter inc)
    (symbol (str "$" name "__" n)))) ;TODO: Something less likely to collide.

(defn lookup [id]
  (or (get-in *module* [:env id])
      (fail (str "undefined: " id) {:id id})))

(defn emit-field [key ast]
  (let [index (-> ast key count)
        path [key index]]
    (when-let [id (:id ast)]
      (change! *module* assoc-in [:env id] path))
    (change! *module* update key conj (assoc ast :index index))
    (change! *module* update :fields conj path)
    index))

(defmulti -modulefield :head)

(defn modulefield [form]
  (-modulefield (parse/modulefield form)))

(defmethod -modulefield 'type [ast]
  (fail "not analyze/-modulefield 'type"))

(defmethod -modulefield 'import [ast]
  (emit-field :imports ast)) ;XXX parseme

(defmethod -modulefield 'func [ast]
  (if-let [{:keys [module name]} (:import ast)]
    (modulefield (list 'import module name
                       (concat ['func]
                               (when-let [id (:id ast)]
                                 [id])
                               (-> ast :type :forms))))
    (if-let [{:keys [name tail]} (:export ast)]
      (let [id (fresh-id name)]
        (modulefield (list 'export name (list 'func id)))
        (modulefield (list* 'func id tail)))
      (emit-field :funcs ast))))

(defmethod -modulefield 'table [ast]
  (fail "not analyze/-modulefield 'table"))

(defmethod -modulefield 'mem [ast]
  (fail "not analyze/-modulefield 'mem"))

(defmethod -modulefield 'global [ast]
  (fail "not analyze/-modulefield 'global"))

(defmethod -modulefield 'export [ast]
  (prn '!!! ast)
  (emit-field :exports ast)) ;XXX parseme

(defmethod -modulefield 'start [ast]
  (fail "not analyze/-modulefield 'start"))

(defmethod -modulefield 'elem [ast]
  (fail "not analyze/-modulefield 'elem"))

(defmethod -modulefield 'data [ast]
  (fail "not analyze/-modulefield 'data"))

;;; Modules.

(defn module [form]
  (let [{:keys [fields] :as ast} (parse/module form)
        ast (assoc ast
                   :env {}
                   :counter 0
                   :fields []
                   :types []
                   :funcs []
                   :imports []
                   :exports [])]
    (binding [*module* ast]
      (run! modulefield fields)
      *module*)))

;;; End.

(comment

  (module '(module))

  (modulefield '(func $i (import "imports" "imported_func") (param i32)))
  (modulefield '(func (export "exported_func") i32.const 42 call $i))

)
