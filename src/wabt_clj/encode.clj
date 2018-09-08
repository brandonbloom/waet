(ns wabt-clj.encode
  (:use [wabt-clj.util])
  (:require [wabt-clj.values :refer [u32?]]
            [wabt-clj.inst :as inst]
            [wabt-clj.io :as io])
  (:import [java.nio.charset Charset StandardCharsets]
           [java.nio ByteBuffer]))

;;;; See <https://webassembly.github.io/spec/core/binary/index.html>.

(def ^:dynamic ^io/WriteSeeker *w*)

(defn pos ^long []
  (io/position *w*))

;;; Scalars.

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

(defn write-u32-leb128 [n]
  {:pre [(u32? n)]}
  (write-unsigned-leb128 n))

;;XXX unused?
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

(def write-index write-u32-leb128)

(def max-u32-leb128-size 5)

(defn write-f32 [value]
  (let [bs (byte-array 4)]
    (.putFloat (ByteBuffer/wrap bs) value)
    (write-bytes bs)))

(defn write-f64 [value]
  (let [bs (byte-array 8)]
    (.putDouble (ByteBuffer/wrap bs) value)
    (write-bytes bs)))

;;; Composites.

(defn reserve-u32-leb128 []
  (let [begin (pos)]
    (dotimes [_ max-u32-leb128-size]
      (write-byte 0x00))
    begin))

(defn begin-section [secid]
  (write-byte ^byte secid)
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

(defmacro writing-section [secid & body]
  `(let [mark# (begin-section ~secid)
         res# (do ~@body)]
     (end-section mark#)
     res#))

(defn begin-subsection []
  (reserve-u32-leb128))

(def end-subsection end-section)

(defmacro writing-subsection [& body]
  `(let [mark# (begin-subsection)
         res# (do ~@body)]
     (end-subsection mark#)
     res#))

(defn write-vec [write xs]
  (write-u32-leb128 (count xs))
  (run! write xs))

(defn write-vecsec [secid write fields]
  (when (seq fields)
    (writing-section secid
      (write-vec write fields))))

(def ^Charset utf8 StandardCharsets/UTF_8)

(defn write-utf8 [^String s]
  (let [^bytes bs (.getBytes s utf8)
        n (count bs)]
    (write-u32-leb128 n)
    (write-bytes bs)))

(defn write-name [name]
  (write-utf8 (str name)))

(defn write-valtype [valtype]
  (write-byte (case valtype
                i32 0x7F
                i64 0x7E
                f32 0x7D
                f64 0x7C)))

(defn write-limits [{:keys [min max]}]
  (if (= min max)
    (do (write-byte 0x00)
        (write-u32-leb128 min)
        (write-u32-leb128 max))
    (do (write-byte 0x01)
        (write-u32-leb128 min))))

;;; Instructions.

(eval
  `(defn ~'opcode [op#]
     (case op#
       ~@(mapcat (juxt :name :opcode) inst/operations)
       (fail (str "unknown operation: " op#) {:op op#}))))

(defn write-opcode [op]
  (write-byte (opcode op)))

(defn write-blocktype [results]
  {:pre [(vector? results)]}
  (cond
    (= results []) (write-byte 0x40)
    (= (count results) 1) (write-valtype (-> results first :type))
    :else (fail "multiple-return blocktype is unsupported")))

(declare write-inst)

(defn write-expr [body]
  (run! write-inst body)
  (write-opcode 'end))

(defn write-block [{:keys [results body] :as block}]
  (write-blocktype results)
  (write-expr body))

(defn write-then+else [{:keys [results then else]}]
  (write-blocktype results)
  (run! write-inst then)
  (when else
    (write-opcode 'else)
    (run! write-inst else))
  (write-opcode 'end))

(defn write-inst [{:keys [op] :as inst}]
  (write-opcode op)
  (case (get-in inst/by-name [op :shape])
    :nullary nil
    :block (write-block inst)
    :if (write-then+else inst)
    :label (write-index (-> inst :label :index))
    ;TODO: :br_table
    :call (write-index (-> inst :func :index))
    :call_indirect (write-index (-> inst :type :index))
    :local (write-index (-> inst :local :index))
    :global (write-index (-> inst :global :index))
    ;TODO: :mem
    :i32 (write-signed-leb128 (:value inst))
    :i64 (write-signed-leb128 (:value inst))
    :f32 (write-f32 (:value inst))
    :f64 (write-f64 (:value inst))
    ))

;;; Types.

(defn write-functype [{:keys [params results]}]
  (write-byte 0x60)
  (write-vec write-valtype (map :type params))
  (write-vec write-valtype (map :type results)))

(defn write-typesec [types]
  (write-vecsec 1 write-functype types))

;;; Imports.

(defn write-importdesc [{:keys [head index] :as desc}]
  (write-byte (case head
                type   0x00
                table  0x01
                mem    0x02
                global 0x03
                (fail "Invalid importdesc head" {:desc desc})))
  (write-u32-leb128 index))

(defn write-import [{:keys [name module desc] :as import}]
  (write-name name)
  (write-name module)
  (write-importdesc desc))

(defn write-importsec [imports]
  (write-vecsec 2 write-import imports))

;;; Functions.

(defn write-type [{:keys [index]}]
  (write-index index))

(defn write-funcsec [funcs]
  (write-vecsec 3 write-type (map :type funcs)))

;;; Tables.

(defn write-elemtype [elemtype]
  (write-byte (case elemtype
                'anyval 0x70)))

(defn write-tabletype [{:keys [type limits]}]
  (write-elemtype type)
  (write-limits limits))

(defn write-tablesec [tables]
  (write-vecsec 4 write-tabletype tables))

;;; Memory.

(def write-memtype write-limits)

(def write-mem write-memtype)

(defn write-memsec [mems]
  (write-vecsec 5 write-mem mems))

;;; Globals.

(defn write-globaltype [{:keys [type mut]}]
  (write-valtype type)
  (case mut
    true (write-byte 0x00)
    false (write-byte 0x01)))

(defn write-global [{:keys [type expr]}]
  (write-globaltype type)
  (write-expr expr))

(defn write-globalsec [globals]
  (write-vecsec 6 write-global globals))

;;; Exports.

(defn write-exportdesc [{:keys [head index] :as desc}]
  (write-byte (case head
                func   0x00
                table  0x01
                mem    0x02
                global 0x03
                (fail "Invalid exportdesc head" {:desc desc})))
  (write-u32-leb128 index))

(defn write-export [{:keys [name desc] :as export}]
  (write-name name)
  (write-exportdesc desc))

(defn write-exportsec [exports]
  (write-vecsec 7 write-export exports))

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

(defn write-elemsec [elems]
  (write-vecsec 9 write-elem elems))

;;; Code.

(defn write-locals [locals]
  (write-vec (fn [[n type]]
               (write-u32-leb128 n)
               (write-valtype type))
             (->> locals
                  (map :type)
                  (partition-by identity)
                  (map (juxt count first)))))

(defn write-code [{:keys [locals body]}]
  (writing-subsection
    (write-locals locals)
    (write-expr body)))

(defn write-codesec [funcs]
  (write-vecsec 10 write-code funcs))

;;; Data.

(defn write-data [{:keys [memory offset init]}]
  (write-index (:index memory))
  (write-expr offset)
  (write-vec write-byte init))

(defn write-datasec [data]
  (write-vecsec 11 write-data data))

;;; Modules.

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

(defn write-module [module]
  (write-magic)
  (write-version)
  ;TODO: custom sections interleaved throughout.
  (write-typesec (-> module :types :fields))
  (write-importsec (-> module :imports :fields))
  (write-funcsec (-> module :funcs :fields))
  (write-tablesec (-> module :tables :fields))
  (write-memsec (-> module :mems :fields))
  (write-globalsec (-> module :globals :fields))
  (write-exportsec (-> module :exports :fields))
  (write-startsec (:start module))
  (write-elemsec (-> module :elems :fields))
  (write-codesec (-> module :funcs :fields))
  (write-datasec (-> module :data :fields)))

(defn encode-module [w module]
  (binding [*w* w]
    (write-module module)))
