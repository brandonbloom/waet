(ns wabt-clj.core
  (:require [wabt-clj.analyze :refer [analyze-module]]
            [wabt-clj.encode :refer [encode-module]]
            [wabt-clj.io :as io]))

(defn edn->wasm [r w]
  (let [module (analyze-module (io/edn-seq r))]
    (encode-module w module)))

(comment

  (let [forms '[

                (func (param i32) (result i32)
                  get_local 0
                  i32.const 0
                  i32.eq
                  if (result i32)
                    i32.const 1
                  else
                    i32.const 2
                  end)

                (func (export "test1") (result i32)
                  i32.const 0
                  call 0)

                (func (export "test2") (result i32)
                  i32.const 1
                  call 0)

                ]]
    (with-open [^java.io.Closeable w (io/open-file-writer "/tmp/scratch.wasm")]
      (edn->wasm forms w)))

)
