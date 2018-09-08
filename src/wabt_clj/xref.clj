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
  (or (get-in *locals* [:locals id])
      (fail (str "undefined local: " id))))

(declare xref-inst)

(defn xref-body [body]
  (mapv xref-inst body))

(defn xref-block [{:keys [label body] :as block}]
  (if label
    (let [label (assoc label :index (count *labels*))]
      (binding [*labels* (assoc *labels* (:id label) label)]
        (-> block
            (assoc :label label)
            (update :body xref-body))))
    (update :body xref-body)))

(defn xref-inst [inst]
  (case (get-in inst/by-name [(:op inst) :shape])
    :nullary inst
    :block (xref-block inst)
    :if (-> inst
            (update :then xref-block)
            (update :else xref-block))
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
  (binding [*locals* (into {} (map (juxt :id identity)) (:locals func))
            *labels* {}]
    (-> func
        (update :type resolved)
        (update :body xref-body))))

(defn xref-elem [module elem]
  (update elem :table resolved))

(defn xref-data [module data]
  (update data :mem resolved))

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
