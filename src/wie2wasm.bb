#!/usr/bin/env bb

;;;; Like WABT's wat2wasm tool, but for wie.

(require '[clojure.tools.cli :refer [parse-opts]])
(require '[clojure.java.io :as io])
(require '[docopt.core :refer [docopt]])
(require '[instaparse.core :as insta])
(require '[fipp.edn :refer [pprint]])
(require '[waet.util :refer :all])
(require '[waet.wat :refer [wat->wie]])
(require '[waet.core :refer [wie->wasm]])
(require '[waet.io :as io])

(def usage "
usage: wie2wasm [options] <filename>

  read a file in the wasm text format, check it for errors, and
  convert it to the wasm binary format.

examples:
  # parse test.wat and write to .wasm binary file with the same name
  $ wie2wasm test.wat

  # parse test.wat and write to binary file test.wasm
  $ wie2wasm test.wat -o test.wasm

  # parse spec-test.wast, and write verbose output to stdout (including
  # the meaning of every byte)
  $ wie2wasm spec-test.wast -v

options:
      --help                                   Print this help message
      --version                                Print version information
  -v, --verbose                                Use multiple times for more info
      --debug-parser                           Turn on debugging the parser of wat files
  -d, --dump-module                            Print a hexdump of the module to stdout
      --enable-exceptions                      Enable Experimental exception handling
      --disable-mutable-globals                Disable Import/export mutable globals
      --disable-saturating-float-to-int        Disable Saturating float-to-int operators
      --disable-sign-extension                 Disable Sign-extension operators
      --disable-simd                           Disable SIMD support
      --enable-threads                         Enable Threading support
      --enable-function-references             Enable Typed function references
      --disable-multi-value                    Disable Multi-value
      --enable-tail-call                       Enable Tail-call support
      --disable-bulk-memory                    Disable Bulk-memory operations
      --disable-reference-types                Disable Reference types (externref)
      --enable-annotations                     Enable Custom annotation syntax
      --enable-code-metadata                   Enable Code metadata
      --enable-gc                              Enable Garbage collection
      --enable-memory64                        Enable 64-bit memory
      --enable-multi-memory                    Enable Multi-memory
      --enable-extended-const                  Enable Extended constant expressions
      --enable-relaxed-simd                    Enable Relaxed SIMD
      --enable-all                             Enable all features
  -o, --output=FILE                            Output wasm binary file. Use \"-\" to write to stdout.
  -r, --relocatable                            Create a relocatable wasm binary (suitable for linking with e.g. lld)
      --no-canonicalize-leb128s                Write all LEB128 sizes as 5-bytes instead of their minimal size
      --debug-names                            Write debug names to the generated binary file
      --no-check                               Don't check for invalid modules
      --wat                                    Accept WebAssembly Text as input (ie. behave as wat2wie2wasm).
")

(def parsed-args (docopt usage (cons "" *command-line-args*)))

(when-not parsed-args
  (System/exit 1))

(def used-args (atom #{}))

(defn get-arg [name]
  (swap! used-args conj name)
  (parsed-args name))

;; Ignore "enable" options; assume "--enable-all".
(get-arg "--enable-exceptions")
(get-arg "--enable-threads")
(get-arg "--enable-function-references")
(get-arg "--enable-tail-call")
(get-arg "--enable-annotations")
(get-arg "--enable-code-metadata")
(get-arg "--enable-gc")
(get-arg "--enable-memory64")
(get-arg "--enable-multi-memory")
(get-arg "--enable-extended-const")
(get-arg "--enable-relaxed-simd")
(get-arg "--enable-all")

;; Read and translate input.
(def filename (get-arg "<filename>"))
(def wie
  (if (get-arg "--wat")
    (-> filename slurp wat->wie)
    (-> filename clojure.java.io/reader)))
(when (insta/failure? wie)
  (fatal wie))

;; Resolve output stream.
(def out
  (if-let [output (get-arg "--output")]
    (clojure.java.io/output-stream output)
    System/out))

;; Check for unsupported arguments.
(doseq [[k v] parsed-args]
  (when (and v (not (@used-args k)))
    (fatal "unsupported argument: " k)))

;; Assemble and write output.
(def aw (io/new-array-writer))
(wie->wasm wie aw)
(io/write-to aw out)
(.close out)
