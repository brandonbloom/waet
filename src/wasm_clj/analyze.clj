(ns wasm-clj.analyze
  (:use [wasm-clj.util])
  (:require [wasm-clj.parse :refer [parse-module]]
            [wasm-clj.xref :refer [xref-module]]
            [wasm-clj.validate :refer [validate-module]]))

(defn analyze-module [form]
  (-> form
      parse-module
      ;(tap #(fipp.edn/pprint (list 'parsed= %)))
      xref-module
      (tap #(fipp.edn/pprint (list 'xrefed= %)))
      validate-module))
