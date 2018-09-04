(ns wabt-clj.core
  (:require [wabt-clj.analyze :refer [analyze-module]]
            [wabt-clj.encode :refer [encode-module]]
            [wabt-clj.io :as io]))

(comment

  (let [form '(module
                (func (export "the_answer") (result i32)
                  i32.const 42)
                (func $main (result i32)
                  i32.const 123)
                (start $main))
        ast (analyze-module form)]
    (with-open [^java.io.Closeable w (io/open-file-writer "/tmp/scratch.wasm")]
      (encode-module w ast)))

)
