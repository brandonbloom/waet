(ns waet.xref
  (:use [waet.util])
  (:require [waet.values :refer [id? index?]]
            [waet.inst :as inst]))

(def ^:dynamic *module*)

(defn resolve-id [section id]
  {:pre [(keyword? section)]}
  (or (get-in *module* [section :env id])
      (fail (str id " undefined in " section) {:section section :id id})))

(defn resolved [{:keys [section id index] :as ast}]
  (cond
    index ast
    (number? id) (assoc ast :index id) ; TODO: Validate consistency of id and index.
    :else (assoc ast :index (resolve-id section id))))

(defn xref-export [export]
  (update export :desc resolved))

(def ^:dynamic *locals*)
(def ^:dynamic *labels*) ; name->index.
(def ^:dynamic *frames*) ; index->label.

(defn resolved-local [{:keys [id] :as local}]
  (or (*locals* id)
      (fail (str "undefined local: " id)
            {:local local :env *locals*})))

(defn resolved-label [{:keys [id] :as label}]
  (if-let [index (if (int? id)
                   (- (count *frames*) id 1)
                   (*labels* id))]
    (assoc (*frames* index) :depth (- (count *frames*) index 1))
    (fail (str "undefined label: " id
               {:label label :env *frames*}))))

(declare xref-inst)

(defn xref-body [body]
  {:pre [(vector? body)]}
  (mapv xref-inst body))

(defn xref-bodies [{:keys [label body] :as ast} keys]
  (let [index (count *frames*)
        {:keys [id] :as label} (assoc label :index index)
        ast (assoc ast :label label)]
    (binding [*labels* (cond-> *labels*
                         id (assoc id index))
              *frames* (conj *frames* label)]
      (reduce (fn [ast key]
                (update ast key xref-body))
              ast
              keys))))

(defn xref-inst [{:keys [op] :as inst}]
  (case (get-in inst/by-name [(:op inst) :immediates])
    :none inst
    :block (xref-bodies inst [:body])
    :if (xref-bodies inst [:then :else])
    :label (update inst :label resolved-label)
    :br_table (-> inst
                  (update :branches #(mapv resolved-label %))
                  (update :default resolved-label))
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
            *labels* {}
            *frames* []]
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
