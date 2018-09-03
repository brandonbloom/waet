(ns wasm-clj.xref
  (:use [wasm-clj.util])
  (:require [wasm-clj.values :refer [id? index?]]))

(def ^:dynamic *module*)

(defn resolve-id [section id]
  (or (get-in *module* [section :env id])
      (fail (str id " undefined in " section) {:section section :id id})))

(defn resolved [{:keys [section id] :as ast}]
  (assoc ast :index (resolve-id section id)))

(defn xref-vecsec [module xref section]
  (update-in module [section :fields] #(mapv xref %)))

(defn xref-start [:begin 'start] [_ start]
  (update start :func resolved))

(defn xref-module [module]
  (binding [*module* module]

    ))
