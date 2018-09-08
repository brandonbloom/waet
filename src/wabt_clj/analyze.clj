(ns wabt-clj.analyze
  (:use [wabt-clj.util])
  (:require [wabt-clj.parse :refer [parse-module]]
            [wabt-clj.xref :refer [xref-module]]
            [wabt-clj.validate :refer [validate-module]]))

(defn analyze-module [forms]
  (-> forms
      parse-module
      ;(tap #(fipp.edn/pprint (list 'parsed= %)))
      xref-module
      ;(tap #(fipp.edn/pprint (list 'xrefed= %)))
      validate-module))
