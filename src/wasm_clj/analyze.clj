(ns wasm-clj.analyze
  (:require [clojure.spec.alpha :as s])
  (:use [wasm-clj.util]))

;;; See https://webassembly.github.io/spec/core/text/index.html

(defn free-name [ctx]
  (gensym)) ;TODO: Something stable.

(defn origin [form]
  (-> (select-keys (meta form) [:line :column])
      (assoc :form form)))

(defn id? [x]
  (and (simple-symbol? x)
       (= (first (str x)) \$)))

(defn name? [x]
  ;;TODO: Tighter validation.
  (string? x))

(defn classify [form]
  (cond
    (int? form) :int
    (id? form) :id
    (seq? form) (first form)
    :else (fail "cannot analyze" {:form form})))

(defmulti -analyze
  (fn [ctx form]
    [(:sort ctx) (classify form)]))

(defn analyze [ctx form]
  (let [origin (origin form)
        asts (-analyze ctx form)
        asts (if (map? asts)
               [asts]
               asts)]
    (map #(merge % origin) asts)))

(defn analyze1 [ctx form]
  (let [asts (analyze ctx form)]
    (when (not= (count asts) 1)
      (fail "unexpected expansion" {:actual asts}))
    (first asts)))

(defn analyze-module [form]
  (analyze {:sort :module} form))

;(defmethod -analyze :int
;  [ctx form]
;  {:head :const
;   :value form})

(defmethod -analyze [:module 'module]
  [ctx [_ & tail]]
  (let [[id & fields] (if (id? (first tail))
                        tail
                        (cons nil tail))
        fields (let [ctx (assoc ctx :sort :modulefield)]
                 (vec (mapcat #(analyze ctx %) fields)))]
    {:head 'module
     :id id
     :fields fields}))

;TODO modulefield: type, import, table, mem, global, export, start, elem, data.

(defn scan-typeuse [tail]
  [[] tail]) ;XXX

(defn scan-locals [tail]
  [[] tail]) ;XXX

(defmethod -analyze [:modulefield 'func]
  [ctx [_ & tail :as form]]
  (let [[id & tail] (if (id? (first tail))
                      tail
                      (cons nil tail))
        macro (and (seq? (first tail))
                   (#{'import 'export} (ffirst tail)))]
    (case macro
      import (let [[import & typeuse] tail]
               (analyze ctx (concat import
                                    [(concat ['func]
                                             (when id [id])
                                             typeuse)])))
      export (let [[export & tail] tail
                   id (or id (free-name ctx))]
               (concat (analyze ctx (concat export [(list 'func id)]))
                       (analyze ctx (list* 'func id tail))))
      (let [[typeuse tail] (scan-typeuse tail)
            [locals tail] (scan-locals tail)
            instructions (let [ctx (assoc ctx :sort :instruction)]
                           tail ;XXX scan-instructions
                           ;(vec (mapcat #(analyze ctx %) tail))
                           )]
        {:head 'func
         :typeuse typeuse
         :locals locals
         :instructions instructions}))))

(defmethod -analyze [:modulefield 'export]
  [ctx [_ & tail :as form]]
  (when (not= (count tail) 2)
    (fail "malformed export" {:form form}))
  (let [[name desc] tail
        _ (when-not (name? name)
            (fail "expected export name" {:form form :actual name}))
        desc (if (not= (count desc) 2)
               (fail "malformed exportdesc" {:form desc})
               (let [[export index] desc]
                 (when-not ('#{func table memory global} export)
                   (fail "invalid export" {:head export}))
                 {:export export
                  :index index}))]
    {:head 'export
     :name name
     :desc desc}))

(defmethod -analyze [:modulefield 'import]
  [ctx [_ & tail :as form]]
  (when (not= (count tail) 3)
    (fail "malformed import" {:form form}))
  (let [[module name desc] tail
        _ (when-not (name? module)
            (fail "expected module name" {:actual module}))
        _ (when-not (name? name)
            (fail "expected import name" {:actual name}))
        desc (let [ctx (assoc ctx :sort :importdesc)]
               (analyze1 ctx desc))]
    {:head 'import
     :module module
     :name name
     :desc desc}))

(defmethod -analyze [:importdesc 'func]
  [ctx form]
  {}) ;XXX

(comment

  (defmacro party [form]
    `(fipp.edn/pprint (analyze-module '~form)))

  (false? (id? 'x))
  (true? (id? '$x))

  ;;TODO Test more [MDN Examples](https://github.com/mdn/webassembly-examples).

  (party
    (module
      (func $i (import "imports" "imported_func") (param i32))
      (func (export "exported_func")
        i32.const 42
        call $i)
      ))

)
