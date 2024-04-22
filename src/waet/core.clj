(ns waet.core
  (:require [waet.analyze :refer [analyze-module]]
            [waet.encode :refer [encode-module]]
            [waet.io :as io]))

(defn wie->wasm [r w]
  (let [module (analyze-module (io/edn-seq r))]
    (encode-module w module)))
