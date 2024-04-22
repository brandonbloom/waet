#!/usr/bin/env bb

;;;; Converts WAT s-expressions into WIE Edn (Clojure's s-exprs).

(require '[waet.wat :refer [wat->wie]])
(require '[clojure.pprint :refer [pprint]])

(->> (slurp *in*)
     wat->wie
     (run! pprint))
