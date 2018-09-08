(ns wabt-clj.core
  (:require [wabt-clj.analyze :refer [analyze-module]]
            [wabt-clj.encode :refer [encode-module]]
            [wabt-clj.io :as io]))

(defn edn->wasm [r w]
  (let [module (analyze-module (io/edn-seq r))]
    (encode-module w module)))

(comment

  (let [forms '[(func (export "the_answer") (result i32)
                  i32.const 42)
                (func $main (result i32)
                  i32.const 123)
                (start $main)]]
    (with-open [^java.io.Closeable w (io/open-file-writer "/tmp/scratch.wasm")]
      (edn->wasm forms w)))

)
