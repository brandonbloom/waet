(ns wasm-clj.ast
  (:require [clojure.spec.alpha :as s]))

(s/def :wasm/valtype '#{i32 i64 f32 f64})

(s/def :wasm/functype
  (s/keys :req-un [:wasm.functype/params
                   :wasm.functype/results]))

(s/def :wasm.functype/params
  (s/coll-of :wasm/valtype :kind vector?))

(s/def :wasm.functype/results
  (s/coll-of :wasm/valtype :kind vector?))

(s/def :wasm/module
  (s/keys :req-un [:wasm.module/types
                   :wasm.module/imports
                   :wasm.module/funcs
                   :wasm.module/tables
                   :wasm.module/mems
                   :wasm.module/globals
                   :wasm.module/exports
                   :wasm.module/start
                   :wasm.module/elems
                   :wasm.module/code
                   :wasm.module/data]))
