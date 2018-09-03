(ns wasm-clj.resolve
  (:use [wasm-clj.util])
  (:require [wasm-clj.values :refer [id? index?]]
            [wasm-clj.visit :refer [visit]]))

(def ^:dynamic *module*)

(defn resolve-id [section id]
  (or (get-in *module* [section :env id])
      (fail (str id " undefined in " section) {:section section :id id})))

(defn validate-index [section index]
  (if (contains? (section *module*) index)
    index
    (fail (str index " out of bounds in " section)
          {:index index
           :section section})))

(defn resolved [{:keys [section id] :as ast}]
  (assoc ast :index (resolve-id section id)))

(defmulti resolve-names
  (fn [event {:keys [sort head]}]
    [event sort head]))

(defn head->section [head]
  (case head
    func :funcs
    export :exports))

(defmethod resolve-names :default [_ {:keys [sort section] :as ast}]
  (prn 'default-resolve-names [sort section])
  (if (= sort :exportdesc)
    (resolved ast)
    ast))

(defn resolve-names-in-module [ast]
  (binding [*module* ast]
    (visit ast resolve-names)))
