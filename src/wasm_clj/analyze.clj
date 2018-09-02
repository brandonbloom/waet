(ns wasm-clj.analyze
  (:use [wasm-clj.util])
  (:require [wasm-clj.parse :refer [parse-module]]
            [wasm-clj.resolve :refer [resolve-ids-in-module]]
            [wasm-clj.validate :refer [validate-module]]))

(defn analyze-module [form]
  (-> form parse-module resolve-ids-in-module validate-module))
