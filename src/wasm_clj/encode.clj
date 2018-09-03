(ns wasm-clj.encode
  (:use [wasm-clj.util])
  (:require [wasm-clj.values :refer [u32?]]
            [wasm-clj.io :as io])
  (:import [java.nio.charset Charset StandardCharsets]))

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

;;; Composites.

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

(defn write-vec-section [id write fields]
  (when (seq fields)
    (writing-section id
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

(defn opcode [op]
  (case op

    ;;; Control Instructions.

    unreachable   0x00
    nop           0x01
    block         0x02
    loop          0x03
    if            0x04
    end           0x0B
    br            0x0C
    br_if         0x0D
    br_table      0x0E
    return        0x0F
    call          0x10
    call_indirect 0x11

    ;;; Parametric Instructions.

    drop   0x1A
    select 0x1B

    ;;; Variable Instructions.

    get_local  0x20
    set_local  0x21
    tee_local  0x22
    get_global 0x23
    set_global 0x24

    ;;; Memory Instructions

    i32.load       0x28
    i64.load       0x29
    f32.load       0x2A
    f64.load       0x2B
    i32.load8_s    0x2C
    i32.load8_u    0x2D
    i32.load16_s   0x2E
    i32.load16_u   0x2F
    i64.load8_s    0x30
    i64.load8_u    0x31
    i64.load16_s   0x32
    i64.load16_u   0x33
    i64.load32_s   0x34
    i64.load32_u   0x35
    i32.store      0x36
    i64.store      0x37
    f32.store      0x38
    f64.store      0x39
    i32.store8     0x3A
    i32.store16    0x3B
    i64.store8     0x3C
    i64.store16    0x3D
    i64.store32    0x3E
    memory.size    0x3F
    memory.grow    0x40

    ;;; Numeric Instructions.

    i32.const  0x41
    i64.const  0x42
    f32.const  0x43
    f64.const  0x44

    i32.eqz   0x45
    i32.eq    0x46
    i32.ne    0x47
    i32.lt_s  0x48
    i32.lt_u  0x49
    i32.gt_s  0x4a
    i32.gt_u  0x4b
    i32.le_s  0x4c
    i32.le_u  0x4d
    i32.ge_s  0x4e
    i32.ge_u  0x4f

    i64.eqz   0x50
    i64.eq    0x51
    i64.ne    0x52
    i64.lt_s  0x53
    i64.lt_u  0x54
    i64.gt_s  0x55
    i64.gt_u  0x56
    i64.le_s  0x57
    i64.le_u  0x58
    i64.ge_s  0x59
    i64.ge_u  0x5a

    f32.eq  0x5b
    f32.ne  0x5c
    f32.lt  0x5d
    f32.gt  0x5e
    f32.le  0x5f
    f32.ge  0x60

    f64.eq  0x61
    f64.ne  0x62
    f64.lt  0x63
    f64.gt  0x64
    f64.le  0x65
    f64.ge  0x66

    i32.clz     0x67
    i32.ctz     0x68
    i32.popcnt  0x69
    i32.add     0x6a
    i32.sub     0x6b
    i32.mul     0x6c
    i32.div_s   0x6d
    i32.div_u   0x6e
    i32.rem_s   0x6f
    i32.rem_u   0x70
    i32.and     0x71
    i32.or      0x72
    i32.xor     0x73
    i32.shl     0x74
    i32.shr_s   0x75
    i32.shr_u   0x76
    i32.rotl    0x77
    i32.rotr    0x78

    i64.clz     0x79
    i64.ctz     0x7a
    i64.popcnt  0x7b
    i64.add     0x7c
    i64.sub     0x7d
    i64.mul     0x7e
    i64.div_s   0x7f
    i64.div_u   0x80
    i64.rem_s   0x81
    i64.rem_u   0x82
    i64.and     0x83
    i64.or      0x84
    i64.xor     0x85
    i64.shl     0x86
    i64.shr_s   0x87
    i64.shr_u   0x88
    i64.rotl    0x89
    i64.rotr    0x8a

    f32.abs       0x8b
    f32.neg       0x8c
    f32.ceil      0x8d
    f32.floor     0x8e
    f32.trunc     0x8f
    f32.nearest   0x90
    f32.sqrt      0x91
    f32.add       0x92
    f32.sub       0x93
    f32.mul       0x94
    f32.div       0x95
    f32.fmin      0x96
    f32.fmax      0x97
    f32.copysign  0x98

    f64.abs       0x99
    f64.neg       0x9a
    f64.ceil      0x9b
    f64.floor     0x9c
    f64.trunc     0x9d
    f64.nearest   0x9e
    f64.sqrt      0x9f
    f64.add       0xa0
    f64.sub       0xa1
    f64.mul       0xa2
    f64.div       0xa3
    f64.fmin      0xa4
    f64.fmax      0xa5
    f64.copysign  0xa6

    i32.wrap/i64         0xa7
    i32.trunc_s/f32      0xa8
    i32.trunc_u/f32      0xa9
    i32.trunc_s/f64      0xaa
    i32.trunc_u/f64      0xab
    i64.extend_s/i32     0xac
    i64.extend_u/i32     0xad
    i64.trunc_s/f32      0xae
    i64.trunc_u/f32      0xaf
    i64.trunc_s/f64      0xb0
    i64.trunc_u/f64      0xb1
    f32.convert_s/i32    0xb2
    f32.convert_u/i32    0xb3
    f32.convert_s/i64    0xb4
    f32.convert_u/i64    0xb5
    f32.demote/f64       0xb6
    f64.convert_s/i32    0xb7
    f64.convert_u/i32    0xb8
    f64.convert_s/i64    0xb9
    f64.convert_u/i64    0xba
    f64.promote/f32      0xbb
    i32.reinterpret/f32  0xbc
    i64.reinterpret/f64  0xbd
    f32.reinterpret/i32  0xbe
    f64.reinterpret/i64  0xbf

    ))

(defn write-opcode [op]
  (write-byte (opcode op)))

(defn write-inst [_]
  (fail "TODO write-inst"))

(defn write-expr [{:keys [body]}]
  (run! write-inst body)
  (write-opcode 'end))

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
  (write-u32-leb128 index))

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

(defn write-elemtype [elemtype]
  (write-byte (case elemtype
                'anyval 0x70)))

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
  (write-u32-leb128 index))

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

(defn write-locals [locals]
  (write-vec (fn [[n type]]
               (write-u32-leb128 n)
               (write-valtype type))
             (->> locals
                  (map :type)
                  (partition-by identity)
                  (map (juxt count first)))))

(defn write-code [{:keys [locals expr]}]
  (writing-subsection
    (write-locals locals)
    (write-expr expr)))

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
  (write-typesec (:types module))
  (write-importsec (:imports module))
  (write-funcsec (:funcs module))
  (write-tablesec (:tables module))
  (write-memsec (:mems module))
  (write-globalsec (:globals module))
  (write-exportsec (:exports module))
  (write-startsec (:start module))
  (write-elemsec (:elems module))
  (write-codesec (:funcs module))
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
