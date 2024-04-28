#!/usr/bin/env bb

;;;; Converts WAT s-expressions into WIE Edn (Clojure's s-exprs).

(require '[waet.util :refer :all])
(require '[fipp.edn :refer [pprint]])
(require '[docopt.core :refer [docopt]])
(require '[instaparse.core :as insta])
(require '[waet.wat :refer [wat->wie]])

(def usage "
usage: wat2wie [<filename>]
")

(def args (docopt usage (cons "" *command-line-args*)))

(def filename (or (args "<filename>") "/dev/stdin"))

(def wat (slurp filename))

(def wie (wat->wie wat))

(when (insta/failure? wie)
  (fatal wie))

(->> wat
     wat->wie
     (run! pprint))
