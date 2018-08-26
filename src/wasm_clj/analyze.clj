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

(defn index? [x]
  (or (id? x)
      (pos-int? x)))

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
;  {:sort :expression XXX
;   :head :const
;   :value form})

(defn scan-optional-id [input]
  (if (id? (first input))
    [(first input) (next input)]
    [nil input]))

(defmethod -analyze [:module 'module]
  [ctx [_ & tail]]
  (let [[id fields] (scan-optional-id tail)
        fields (let [ctx (assoc ctx :sort :modulefield)]
                 (vec (mapcat #(analyze ctx %) fields)))]
    {:sort :module
     :head 'module
     :id id
     :fields fields}))

;TODO modulefield: type, import, table, mem, global, export, start, elem, data.

(defn has-head? [head form]
  (and (seq? form)
       (= (first form) head)))

(defn analyze-valtype [form]
  (when-not ('#{i32 i64 f32 f64} form)
    (fail "invalid valtype" {:form form}))
  {:sort :valtype
   :valtype form})

(defn analyze-params [[head & tail]]
  {:pre [(= head 'param)]}
  (let [[id tail] (scan-optional-id tail)]
    (if id
      (let [[valtype & tail] tail]
        (when-first [x tail]
          (fail "malformed param" {:at x}))
        [{:head 'param
          :id id
          :valtype (analyze-valtype valtype)}])
      (mapv (fn [x]
              {:head 'param
               :valtype (analyze-valtype x)})
            tail))))

(defn analyze-results [[head & tail]]
  {:pre [(= head 'result)]}
  (mapv (fn [x]
          {:head 'result
           :valtype (analyze-valtype x)})
        tail))

(defn scan-params [[x & xs :as input]]
  (if (has-head? x 'param)
    [(analyze-params x) xs]
    [nil input]))

(defn scan-results [[x & xs :as input]]
  (prn input)
  (if (has-head? x 'result)
    [(analyze-results x) xs]
    [nil input]))

(defn analyze-params-and-results [input]
  (let [[params input] (loop [params []
                              input input]
                         (let [[params* input] (scan-params input)]
                           (if params*
                             (recur (into params params*) input)
                             [params input])))
        [results input] (loop [results []
                               input input]
                          (let [[results* input] (scan-results input)]
                            (if results*
                              (recur (into results results*) input)
                              (do (when (seq input)
                                    (fail "unexpected" {:form (first input)}))
                                  [results input]))))]
    {:params params
     :results results}))

(defn analyze-typeuse [[head index & extra :as form]]
  {:pre [(= head 'type)]}
  (when (some? extra)
    (fail "malformed typeuse" {:form form}))
  (when-not (index? index)
    (fail "expected index" {:form form :actual index}))
  {:head 'type
   :index index})

(defn scan-typeuse [[x & xs :as input]]
  (prn (list 'scan-typeuse input))
  (if (has-head? 'type x)
    (let [typeuse (analyze-typeuse (first x))]
      (if (seq xs)
        (merge typeuse (analyze-params-and-results xs))
        typeuse))
    (analyze-params-and-results input)))

(defn scan-locals [input]
  [[] input]) ;XXX

(defmethod -analyze [:modulefield 'func]
  [ctx [_ & tail :as form]]
  (let [[id tail] (scan-optional-id tail)
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
        {:sort :modulefield
         :head 'func
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
    {:sort :modulefield
     :head 'export
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
    {:sort :modulefield
     :head 'import
     :module module
     :name name
     :desc desc}))

(defmethod -analyze [:importdesc 'func]
  [ctx [_ & tail :as form]]
  (let [[id tail] (scan-optional-id form)
        [typeuse tail] (scan-typeuse tail)]
    (when (seq tail)
      (fail "expected end of form" {:form form
                                    :at (first tail)}))
    {:sort :importdesc
     :head 'func
     :id id
     :func typeuse}))

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
