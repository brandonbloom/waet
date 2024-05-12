(ns waet.interp
  (:require [clojure.java.shell :refer [sh]]
            [waet.util :refer :all]
            [waet.analyze :refer [analyze-module]]
            [waet.encode :refer [encode-module]]
            [waet.io :as io]))

(defn exec-wasm-path [path]
  (sh "wasm-interp" "--wasi" path)
  ;(sh "bun" "run" path)
  )

(do #?@(:bb [] :clj [

(defn exec-module [module]
  (let [tmpfile (java.io.File/createTempFile "waet-exec" ".wasm")]
    (try
      (with-open [w ^java.io.Closeable (io/open-file-writer tmpfile)]
        (encode-module w module)
        (exec-wasm-path (.getAbsolutePath tmpfile)))
      (finally
        (.delete tmpfile)))))

]))

(comment

  (-> '[

      (module

        (import "wasi_snapshot_preview1" "proc_exit"
          (func $exit (param i32)))

        (memory (export "memory") 0)

        (func (export "_start")
          i32.const 42
          call $exit)

        )]
    analyze-module
    exec-module
    pprint
    )

)
