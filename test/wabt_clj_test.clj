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

(def test-files
  (->> interp-tests-dir ;TODO: Others?
       file-seq
       (remove #(.isDirectory ^File %))
       (filter #(str/ends-with? (.getPath ^File %) ".txt"))))

(comment

  (doseq [^File file (->> test-files
                          ;;XXX remove drop/take
                          (take 29)
                          ;(drop 29) (take 1)
                          )
          :let [_ (println)
                path (.getPath file)
                [tool-line args-line] (line-seq (io/reader file))]
          :when (or (= tool-line ";;; TOOL: run-interp")
                    #_(println "Skipping" path "with unknown TOOL:" tool-line))
          :when (or (not (re-find #"ARG" args-line))
                    #_(println "Skipping" path "with unsupported args:" args-line))
          :let [_ (println "Running test:" path)
                content (slurp file)
                source (-> content
                           ;; Strip WAT-style comments.
                           (str/replace #"(?s)\(;.*;\)" "")
                           ;; Rewrite hex strings in to byte vectors.
                           (str/replace #"(?i)\"(\\[0-9a-f]{2})" "[$1")
                           (str/replace #"(?i)(\\[0-9a-f]{2})\"" "$1]")
                           (str/replace #"(?i)\\([0-9a-f][0-9a-f])" "0x$1,"))
                ;_ (do (println "Source:")
                ;      (println source))
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
      (println)
      (println "source:")
      (println source)
      (println)
      (println "expected:")
      (println output)
      (println)
      (println "actual:")
      (println (:out res))))

)
