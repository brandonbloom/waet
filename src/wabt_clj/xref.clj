(ns wabt-clj.xref
  (:use [wabt-clj.util])
  (:require [wabt-clj.values :refer [id? index?]]
            [wabt-clj.inst :as inst]))

(def ^:dynamic *module*)

(defn resolve-id [section id]
  {:pre [(keyword? section)]}
  (or (get-in *module* [section :env id])
      (fail (str id " undefined in " section) {:section section :id id})))

(defn resolved [{:keys [section id index] :as ast}]
  (if index
    ast
    (assoc ast :index (resolve-id section id))))

(defn xref-export [export]
  (update export :desc resolved))

(def ^:dynamic *locals*)
(def ^:dynamic *labels*)

(defn resolved-local [{:keys [id]}]
  (or (*locals* id)
      (fail (str "undefined local: " id))))

(declare xref-inst)

(defn xref-body [body]
  (mapv xref-inst body))

(defn xref-bodies [{:keys [label body] :as ast} keys]
  (if label
    (let [label (assoc label :index (count *labels*))]
      (binding [*labels* (assoc *labels* (:id label) label)]
        (let [ast (assoc ast :label label)]
          (reduce (fn [ast key]
                    (update ast key xref-body))
                  ast
                  keys))))
    (update :body xref-body)))

(defn xref-inst [inst]
  (case (get-in inst/by-name [(:op inst) :shape])
    :nullary inst
    :block (xref-bodies inst [:body])
    :if (xref-bodies inst [:then :else])
    :label (update inst :label resolved)
    ;XXX :br_table
    :call (update inst :func resolved)
    :call_indirect (update inst :type resolved)
    :local (update inst :local resolved-local)
    :global (update inst :global resolved)
    :mem inst
    :i32 inst
    :i64 inst
    :f32 inst
    :f64 inst
    ))

(defn xref-func [func]
  (binding [*locals* (into {}
                       (mapcat (fn [{:keys [id] :as local} index]
                                 (let [local (assoc local :index index)]
                                   (cons [index local]
                                         (when id
                                           [[id local]]))))
                               (concat (-> func :type :params)
                                       (:locals func))
                               (range)))
            *labels* {}]
    (-> func
        (update :type resolved)
        (update :body xref-body))))

(defn xref-elem [elem]
  (update elem :table resolved))

(defn xref-data [data]
  (update data :memory resolved))

(defn xref-vecsec [module xref section]
  (update-in module [section :fields] #(mapv xref %)))

(defn xref-module [module]
  (binding [*module* module]
    (change! *module* xref-vecsec xref-export :exports)
    (change! *module* xref-vecsec xref-func :funcs)
    (change! *module* xref-vecsec xref-elem :elems)
    (change! *module* xref-vecsec xref-data :data)
    (when (:start *module*)
      (change! *module* update-in [:start :func] resolved))
    *module*))
