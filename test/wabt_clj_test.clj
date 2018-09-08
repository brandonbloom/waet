(ns wabt-clj-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.java.shell :refer [sh]]
            [wabt-clj.core :refer :all]
            [wabt-clj.io :as wabt-io])
  (:import [java.io File Closeable]))

(def wabt-root
  (io/file "/Users/brandonbloom/Projects/wabt")) ;XXX use environment.

(def wasm-interp
  (io/file wabt-root "bin" "wasm-interp"))

(def interp-tests-dir
  (io/file wabt-root "test" "interp"))

;(deftest a-test
;  (testing "FIXME, I fail."
;    (is (= 0 1))))

(comment

  (def test-files
    (->> interp-tests-dir ;TODO: Others?
         file-seq
         (remove #(.isDirectory ^File %))
         (filter #(str/ends-with? (.getPath ^File %) ".txt"))))

  (doseq [^File file (take 10 test-files) ;XXX remove take
          :let [_ (println)
                path (.getPath file)
                [tool-line args-line] (line-seq (io/reader file))]
          :when (or (= tool-line ";;; TOOL: run-interp")
                    (println "Skipping" path "with unknown TOOL:" tool-line))
          :when (or (not (re-find #"ARG" args-line))
                    (println "Skipping" path "with unsupported args:" args-line))
          :let [_ (println "Running test:" path)
                content (slurp file)
                ;; Gather input; strip WAT-style comments.
                source (str/replace content #"(?s)\(;.*;\)" "")
                _ (do (println "Source:")
                      (println source))
                ;; Gather expected output.
                output (->> content
                            (re-find #"(?s)\(;; STDOUT ;;;\n(.*);;; STDOUT ;;\)")
                            second
                            str)
                ;; Compile WIE to Wasm.
                tempfile (.getPath (File/createTempFile "output" ".wasm"))
                _ (with-open [^Closeable w (wabt-io/open-file-writer tempfile)]
                    (edn->wasm source w))
                _ (println "Wrote" tempfile)
                ;; Run WASM.
                res (sh (.getPath ^File wasm-interp)
                        tempfile
                        "--run-all-exports")]]
    (when (not= (:err res) "")
      (println "STDERR:" (:err res)))
    (when (not= (:out res) output)
      (println "OUTPUT DID NOT MATCH")
      (println "expected:" (pr-str output))
      (println "actual:" (pr-str (:out res)))))

)
