(ns wabt-clj.core
  (:require [wabt-clj.analyze :refer [analyze-module]]
            [wabt-clj.encode :refer [encode-module]]
            [wabt-clj.io :as io]))

(defn edn->wasm [r w]
  (let [module (analyze-module (io/edn-seq r))]
    (encode-module w module)))

(comment

  (let [forms '[

  (memory 1)
  (func $store_unless (param i32)
    get_local 0
    i32.const 0
    i32.eq
    if
      return
    end
    i32.const 0
    i32.const 1
    i32.store)

  (func (export "test1")
    i32.const 0
    call $store_unless)

  (func (export "check1") (result i32)
    i32.const 0
    i32.load)

  (func (export "test2")
    i32.const 1
    call $store_unless)

  (func (export "check2") (result i32)
    i32.const 0
    i32.load)

                ]]
    (with-open [^java.io.Closeable w (io/open-file-writer "/tmp/scratch.wasm")]
      (edn->wasm forms w)))

)
