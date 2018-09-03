(ns wasm-clj.analyze
  (:use [wasm-clj.util])
  (:require [wasm-clj.parse :refer [parse-module]]
            [wasm-clj.resolve :refer [resolve-names-in-module]]
            [wasm-clj.validate :refer [validate-module]]))

(defn analyze-module [form]
  (-> form
      parse-module
      ;(tap #(fipp.edn/pprint (list 'parsed= %)))
      resolve-names-in-module
      (tap #(fipp.edn/pprint (list 'resolved= %)))
      validate-module))
