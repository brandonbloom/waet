(ns wabt-clj.inst)

(def header [:opcode :name :shape])
(def table '[

  ;;; Control Instructions.

  0x00 unreachable    :nullary
  0x01 nop            :nullary

  0x02 block          :block
  0x03 loop           :block
  0x04 if             :if
  0x05 else           :nullary
  0x0B end            :nullary

  0x0C br             :label
  0x0D br_if          :label
  0x0E br_table       :br_table
  0x0F return         :nullary
  0x10 call           :call
  0x11 call_indirect  :call_indirect

  ;;; Parametric Instructions.

  0x1A drop    :nullary
  0x1B select  :nullary

  ;;; Variable Instructions.

  0x20 get_local   :local
  0x21 set_local   :local
  0x22 tee_local   :local
  0x23 get_global  :global
  0x24 set_global  :global

  ;;; Memory Instructions

  0x28 i32.load       [:mem 4]
  0x29 i64.load       [:mem 8]
  0x2A f32.load       [:mem 4]
  0x2B f64.load       [:mem 8]
  0x2C i32.load8_s    [:mem 1]
  0x2D i32.load8_u    [:mem 1]
  0x2E i32.load16_s   [:mem 2]
  0x2F i32.load16_u   [:mem 2]
  0x30 i64.load8_s    [:mem 1]
  0x31 i64.load8_u    [:mem 1]
  0x32 i64.load16_s   [:mem 2]
  0x33 i64.load16_u   [:mem 2]
  0x34 i64.load32_s   [:mem 4]
  0x35 i64.load32_u   [:mem 4]
  0x36 i32.store      [:mem 4]
  0x37 i64.store      [:mem 8]
  0x38 f32.store      [:mem 4]
  0x39 f64.store      [:mem 8]
  0x3A i32.store8     [:mem 1]
  0x3B i32.store16    [:mem 2]
  0x3C i64.store8     [:mem 1]
  0x3D i64.store16    [:mem 2]
  0x3E i64.store32    [:mem 4]
  0x3F memory.size    :nullary
  0x40 memory.grow    :nullary

  ;;; Numeric Instructions.

  0x41 i32.const  :i32
  0x42 i64.const  :i64
  0x43 f32.const  :f32
  0x44 f64.const  :f64

  0x45 i32.eqz   :nullary
  0x46 i32.eq    :nullary
  0x47 i32.ne    :nullary
  0x48 i32.lt_s  :nullary
  0x49 i32.lt_u  :nullary
  0x4a i32.gt_s  :nullary
  0x4b i32.gt_u  :nullary
  0x4c i32.le_s  :nullary
  0x4d i32.le_u  :nullary
  0x4e i32.ge_s  :nullary
  0x4f i32.ge_u  :nullary

  0x50 i64.eqz   :nullary
  0x51 i64.eq    :nullary
  0x52 i64.ne    :nullary
  0x53 i64.lt_s  :nullary
  0x54 i64.lt_u  :nullary
  0x55 i64.gt_s  :nullary
  0x56 i64.gt_u  :nullary
  0x57 i64.le_s  :nullary
  0x58 i64.le_u  :nullary
  0x59 i64.ge_s  :nullary
  0x5a i64.ge_u  :nullary

  0x5b f32.eq  :nullary
  0x5c f32.ne  :nullary
  0x5d f32.lt  :nullary
  0x5e f32.gt  :nullary
  0x5f f32.le  :nullary
  0x60 f32.ge  :nullary

  0x61 f64.eq  :nullary
  0x62 f64.ne  :nullary
  0x63 f64.lt  :nullary
  0x64 f64.gt  :nullary
  0x65 f64.le  :nullary
  0x66 f64.ge  :nullary

  0x67 i32.clz     :nullary
  0x68 i32.ctz     :nullary
  0x69 i32.popcnt  :nullary
  0x6a i32.add     :nullary
  0x6b i32.sub     :nullary
  0x6c i32.mul     :nullary
  0x6d i32.div_s   :nullary
  0x6e i32.div_u   :nullary
  0x6f i32.rem_s   :nullary
  0x70 i32.rem_u   :nullary
  0x71 i32.and     :nullary
  0x72 i32.or      :nullary
  0x73 i32.xor     :nullary
  0x74 i32.shl     :nullary
  0x75 i32.shr_s   :nullary
  0x76 i32.shr_u   :nullary
  0x77 i32.rotl    :nullary
  0x78 i32.rotr    :nullary

  0x79 i64.clz     :nullary
  0x7a i64.ctz     :nullary
  0x7b i64.popcnt  :nullary
  0x7c i64.add     :nullary
  0x7d i64.sub     :nullary
  0x7e i64.mul     :nullary
  0x7f i64.div_s   :nullary
  0x80 i64.div_u   :nullary
  0x81 i64.rem_s   :nullary
  0x82 i64.rem_u   :nullary
  0x83 i64.and     :nullary
  0x84 i64.or      :nullary
  0x85 i64.xor     :nullary
  0x86 i64.shl     :nullary
  0x87 i64.shr_s   :nullary
  0x88 i64.shr_u   :nullary
  0x89 i64.rotl    :nullary
  0x8a i64.rotr    :nullary

  0x8b f32.abs       :nullary
  0x8c f32.neg       :nullary
  0x8d f32.ceil      :nullary
  0x8e f32.floor     :nullary
  0x8f f32.trunc     :nullary
  0x90 f32.nearest   :nullary
  0x91 f32.sqrt      :nullary
  0x92 f32.add       :nullary
  0x93 f32.sub       :nullary
  0x94 f32.mul       :nullary
  0x95 f32.div       :nullary
  0x96 f32.fmin      :nullary
  0x97 f32.fmax      :nullary
  0x98 f32.copysign  :nullary

  0x99 f64.abs       :nullary
  0x9a f64.neg       :nullary
  0x9b f64.ceil      :nullary
  0x9c f64.floor     :nullary
  0x9d f64.trunc     :nullary
  0x9e f64.nearest   :nullary
  0x9f f64.sqrt      :nullary
  0xa0 f64.add       :nullary
  0xa1 f64.sub       :nullary
  0xa2 f64.mul       :nullary
  0xa3 f64.div       :nullary
  0xa4 f64.fmin      :nullary
  0xa5 f64.fmax      :nullary
  0xa6 f64.copysign  :nullary

  0xa7 i32.wrap/i64         :nullary
  0xa8 i32.trunc_s/f32      :nullary
  0xa9 i32.trunc_u/f32      :nullary
  0xaa i32.trunc_s/f64      :nullary
  0xab i32.trunc_u/f64      :nullary
  0xac i64.extend_s/i32     :nullary
  0xad i64.extend_u/i32     :nullary
  0xae i64.trunc_s/f32      :nullary
  0xaf i64.trunc_u/f32      :nullary
  0xb0 i64.trunc_s/f64      :nullary
  0xb1 i64.trunc_u/f64      :nullary
  0xb2 f32.convert_s/i32    :nullary
  0xb3 f32.convert_u/i32    :nullary
  0xb4 f32.convert_s/i64    :nullary
  0xb5 f32.convert_u/i64    :nullary
  0xb6 f32.demote/f64       :nullary
  0xb7 f64.convert_s/i32    :nullary
  0xb8 f64.convert_u/i32    :nullary
  0xb9 f64.convert_s/i64    :nullary
  0xba f64.convert_u/i64    :nullary
  0xbb f64.promote/f32      :nullary
  0xbc i32.reinterpret/f32  :nullary
  0xbd i64.reinterpret/f64  :nullary
  0xbe f32.reinterpret/i32  :nullary
  0xbf f64.reinterpret/i64  :nullary

])

(defn parse-row [row]
  (let [{:keys [shape] :as operation} (into {} (mapv vector header row))]
    (if (vector? shape)
      (assoc operation :shape (first shape) :align (last shape))
      operation)))

(def operations
  (->> (partition (count header) table)
       (mapv parse-row)))

(def by-name
  (into {} (map (juxt :name identity)) operations))
