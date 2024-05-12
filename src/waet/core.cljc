(ns waet.core
  (:require [clojure.java.shell :refer [sh]]
            [waet.util :refer :all]
            [waet.analyze :refer [analyze-module]]
            [waet.encode :refer [encode-module]]
            [waet.interp :as interp]
            [waet.io :as io]))

(defn wie->wasm [r w]
  (let [module (analyze-module (io/edn-seq r))]
    (encode-module w module)))

(do #?@(:bb [] :clj [

(def exec-module interp/exec-module)

]))
