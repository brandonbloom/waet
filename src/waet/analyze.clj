(ns waet.analyze
  (:use [waet.util])
  (:require [waet.parse :refer [parse-module]]
            [waet.xref :refer [xref-module]]
            [waet.validate :refer [validate-module]]))

(defn analyze-module [forms]
  (-> forms
      parse-module
      ;(tap #(fipp.edn/pprint (list 'parsed= %)))
      xref-module
      ;(tap #(fipp.edn/pprint (list 'xrefed= %)))
      validate-module))
