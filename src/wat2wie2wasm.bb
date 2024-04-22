#!/usr/bin/env bb

;;;; Emulates WABT's wat2wasm tool, but takes an intermediate
;;;; step through WIE (Wat In Edn) for the sake of testing.

(require '[waet.wat :refer [wat->wie]])
(require '[waet.core :refer [wie->wasm]])
(require '[waet.io :as io])

(def wie (-> *in* slurp wat->wie))

(def aw (io/new-array-writer))
(wie->wasm wie aw)

(io/write-to aw System/out)
