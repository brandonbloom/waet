(ns wasm-clj.encode
  (:use [wasm-clj.util])
  (:require [wasm-clj.io :as io])
  (:import [java.nio.charset Charset StandardCharsets]))

(def ^:dynamic ^io/WriteSeeker *w*)

(defn write-byte [^long b]
  (io/write-byte *w* b))

(defn write-bytes [^bytes bs]
  (io/write-bytes *w* bs))

(defn leb128-write-loop [shift-right until n]
  (loop [^long n n]
    (let [b (bit-and n 0x7F)
          n (shift-right n 7)]
      (if (until n b)
        (write-byte b)
        (do (write-byte (bit-or b 0x80))
            (recur n))))))

(defn write-unsigned-leb128 [n]
  (leb128-write-loop
    unsigned-bit-shift-right
    (fn [^long n, ^long b]
      (zero? n))
    n))

(defn write-signed-leb128 [^long n]
  (leb128-write-loop
    bit-shift-right
    (if (neg? n)
      (fn [n b]
        (and (= n -1)
             (bit-test b 6)))
      (fn [n b]
        (and (zero? ^long n)
             (not (bit-test b 6)))))
    n))

(def write-index write-unsigned-leb128)

(defn pos ^long []
  (io/position *w*))

(def max-u32-leb128-size 5)

(defn reserve-u32-leb128 []
  (let [begin (pos)]
    (dotimes [_ max-u32-leb128-size]
      (write-byte 0x00))
    begin))

(defn begin-section [id]
  (write-byte ^byte id)
  (reserve-u32-leb128))

(defn write-padded-u32-leb128 [^long n]
  (write-byte (bit-or (bit-and n 0x7F) 0x80))
  (write-byte (bit-or (bit-and (unsigned-bit-shift-right n 7) 0x7F) 0x80))
  (write-byte (bit-or (bit-and (unsigned-bit-shift-right n 14) 0x7F) 0x80))
  (write-byte (bit-or (bit-and (unsigned-bit-shift-right n 21) 0x7F) 0x80))
  (write-byte (bit-and (unsigned-bit-shift-right n 28) 0x7F)))

(defn end-section [^long mark]
  (let [end (pos)
        size (- end mark ^long max-u32-leb128-size)]
    (io/seek *w* mark)
    (write-padded-u32-leb128 size)
    (io/seek *w* end)))

(defmacro writing-section [id & body]
  `(let [mark# (begin-section ~id)
         res# (do ~@body)]
     (end-section mark#)
     res#))

(defn write-vec [write xs]
  (write-unsigned-leb128 (count xs))
  (run! write xs))

(defn write-vec-section [id write fields]
  (when (seq fields)
    (writing-section id
      (write-vec write fields))))

(def ^Charset utf8 StandardCharsets/UTF_8)

(defn write-utf8 [^String s]
  (let [^bytes bs (.getBytes s utf8)
        n (count bs)]
    (write-unsigned-leb128 n)
    (write-bytes bs)))

(defn write-name [name]
  (write-utf8 (str name)))

(defn write-magic []
  (write-byte 0x00)
  (write-byte 0x61)
  (write-byte 0x73)
  (write-byte 0x6D))

(defn write-version []
  (write-byte 0x01)
  (write-byte 0x00)
  (write-byte 0x00)
  (write-byte 0x00))

(defn write-valtype [valtype]
  (write-byte (case valtype
                i32 0x7F
                i64 0x7E
                f32 0x7D
                f64 0x7C)))

(defn write-limits [{:keys [min max]}]
  (if (= min max)
    (do (write-byte 0x00)
        (write-unsigned-leb128 min)
        (write-unsigned-leb128 max))
    (do (write-byte 0x01)
        (write-unsigned-leb128 min))))

(defn write-expr [_]
  (fail "TODO"))

(defn write-elemtype [elemtype]
  (write-byte (case elemtype
                'anyval 0x70)))

;;; Types.

(defn write-functype [{:keys [params results]}]
  (write-byte 0x60)
  (write-vec write-valtype params)
  (write-vec write-valtype results))

(defn write-typesec [{:keys [fields]}]
  (write-vec-section 1 write-functype fields))

;;; Imports.

(defn write-importdesc [{:keys [head index] :as desc}]
  (write-byte (case head
                type   0x00
                table  0x01
                mem    0x02
                global 0x03
                (fail "Invalid importdesc head" {:desc desc})))
  (write-unsigned-leb128 index))

(defn write-import [{:keys [name module desc] :as import}]
  (write-name name)
  (write-name module)
  (write-importdesc desc))

(defn write-importsec [{:keys [fields]}]
  (write-vec-section 2 write-import fields))

;;; Functions.

(defn write-type [{:keys [index]}]
  (write-index index))

(defn write-funcsec [{:keys [fields]}]
  (prn 'funcsec-types= (map :type fields))
  (write-vec-section 3 write-type (map :type fields)))

;;; Tables.

(defn write-tabletype [{:keys [type limits]}]
  (write-elemtype type)
  (write-limits limits))

(defn write-tablesec [{:keys [fields]}]
  (write-vec-section 4 write-tabletype fields))

;;; Memory.

(def write-memtype write-limits)

(def write-mem write-memtype)

(defn write-memsec [{:keys [fields]}]
  (write-vec-section 5 write-mem fields))

;;; Globals.

(defn write-globaltype [{:keys [type mut]}]
  (write-valtype type)
  (case mut
    true (write-byte 0x00)
    false (write-byte 0x01)))

(defn write-global [{:keys [type expr]}]
  (write-globaltype type)
  (write-expr expr))

(defn write-globalsec [{:keys [fields]}]
  (write-vec-section 6 write-global fields))

;;; Exports.

(defn write-exportdesc [{:keys [head index] :as desc}]
  (write-byte (case head
                func   0x00
                table  0x01
                mem    0x02
                global 0x03
                (fail "Invalid exportdesc head" {:desc desc})))
  (write-unsigned-leb128 index))

(defn write-export [{:keys [name desc] :as export}]
  (write-name name)
  (write-exportdesc desc))

(defn write-exportsec [{:keys [fields]}]
  (write-vec-section 7 write-export fields))

;;; Start.

(defn write-startsec [start]
  (when-let [{:keys [func]} start]
    (writing-section 8
      (write-index (:index func)))))

;;; Elements.

(defn write-elem [{:keys [table offset init]}]
  (write-index (:index table))
  (write-expr offset)
  (write-vec write-index (map :index init)))

(defn write-elemsec [{:keys [fields]}]
  (write-vec-section 9 write-elem fields))

;;; Code.

(defn write-code [_]
  (fail "TODO"))

(defn write-codesec [{:keys [fields]}]
  (write-vec-section 10 write-code fields))

;;; Data.

(defn write-data [{:keys [memory offset init]}]
  (write-index (:index memory))
  (write-expr offset)
  (write-vec write-byte init))

(defn write-datasec [{:keys [fields]}]
  (write-vec-section 11 write-data fields))

;;; Modules.

(defn write-module [module]
  (write-magic)
  (write-version)
  ;TODO: custom sections interleaved throughout.
  (write-typesec (:types module))
  (write-importsec (:imports module))
  (write-funcsec (:funcs module))
  (write-tablesec (:tables module))
  (write-memsec (:mems module))
  (write-globalsec (:globals module))
  (write-exportsec (:exports module))
  (write-startsec (:start module))
  (write-elemsec (:elems module))
  (write-codesec (:code module))
  (write-datasec (:data module)))

(comment

  (require '[wasm-clj.analyze :refer [analyze-module]])
  (with-open [^java.io.Closeable w (io/open-file-writer "/tmp/scratch.wasm")]
    (binding [*w* w]
      (let [form '(module
                    (func (export "the_answer") (result i32)
                      i32.const 42))
            ast (analyze-module form)]
        (write-module ast))))

)
