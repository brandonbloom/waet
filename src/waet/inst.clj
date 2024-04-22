(ns waet.inst)

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
  0x4A i32.gt_s  :nullary
  0x4B i32.gt_u  :nullary
  0x4C i32.le_s  :nullary
  0x4D i32.le_u  :nullary
  0x4E i32.ge_s  :nullary
  0x4F i32.ge_u  :nullary

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
  0x5A i64.ge_u  :nullary

  0x5B f32.eq  :nullary
  0x5C f32.ne  :nullary
  0x5D f32.lt  :nullary
  0x5E f32.gt  :nullary
  0x5F f32.le  :nullary
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
  0x6A i32.add     :nullary
  0x6B i32.sub     :nullary
  0x6C i32.mul     :nullary
  0x6D i32.div_s   :nullary
  0x6E i32.div_u   :nullary
  0x6F i32.rem_s   :nullary
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
  0x7A i64.ctz     :nullary
  0x7B i64.popcnt  :nullary
  0x7C i64.add     :nullary
  0x7D i64.sub     :nullary
  0x7E i64.mul     :nullary
  0x7F i64.div_s   :nullary
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
  0x8A i64.rotr    :nullary

  0x8B f32.abs       :nullary
  0x8C f32.neg       :nullary
  0x8D f32.ceil      :nullary
  0x8E f32.floor     :nullary
  0x8F f32.trunc     :nullary
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
  0x9A f64.neg       :nullary
  0x9B f64.ceil      :nullary
  0x9C f64.floor     :nullary
  0x9D f64.trunc     :nullary
  0x9E f64.nearest   :nullary
  0x9F f64.sqrt      :nullary
  0xA0 f64.add       :nullary
  0xA1 f64.sub       :nullary
  0xA2 f64.mul       :nullary
  0xA3 f64.div       :nullary
  0xA4 f64.fmin      :nullary
  0xA5 f64.fmax      :nullary
  0xA6 f64.copysign  :nullary

  0xA7 i32.wrap/i64         :nullary
  0xA8 i32.trunc_s/f32      :nullary
  0xA9 i32.trunc_u/f32      :nullary
  0xAA i32.trunc_s/f64      :nullary
  0xAB i32.trunc_u/f64      :nullary
  0xAC i64.extend_s/i32     :nullary
  0xAD i64.extend_u/i32     :nullary
  0xAE i64.trunc_s/f32      :nullary
  0xAF i64.trunc_u/f32      :nullary
  0xB0 i64.trunc_s/f64      :nullary
  0xB1 i64.trunc_u/f64      :nullary
  0xB2 f32.convert_s/i32    :nullary
  0xB3 f32.convert_u/i32    :nullary
  0xB4 f32.convert_s/i64    :nullary
  0xB5 f32.convert_u/i64    :nullary
  0xB6 f32.demote/f64       :nullary
  0xB7 f64.convert_s/i32    :nullary
  0xB8 f64.convert_u/i32    :nullary
  0xB9 f64.convert_s/i64    :nullary
  0xBA f64.convert_u/i64    :nullary
  0xBB f64.promote/f32      :nullary
  0xBC i32.reinterpret/f32  :nullary
  0xBD i64.reinterpret/f64  :nullary
  0xBE f32.reinterpret/i32  :nullary
  0xBF f64.reinterpret/i64  :nullary

  ;;; Threading.

  0xFE00 memory.atomic.notify [:mem 4]
  0xFE01 memory.atomic.wait32 [:mem 4]
  0xFE02 memory.atomic.wait64 [:mem 8]

  0xFE0300 atomic.fence :nullary

  0xFE10   i32.atomic.load     [:mem 4]
  0xFE11   i64.atomic.load     [:mem 8]
  0xFE12   i32.atomic.load8_u  [:mem 1]
  0xFE13   i32.atomic.load16_u [:mem 2]
  0xFE14   i64.atomic.load8_u  [:mem 1]
  0xFE15   i64.atomic.load16_u [:mem 2]
  0xFE16   i64.atomic.load32_u [:mem 4]
  0xFE17   i32.atomic.store    [:mem 4]
  0xFE18   i64.atomic.store    [:mem 8]
  0xFE19   i32.atomic.store8   [:mem 1]
  0xFE1A   i32.atomic.store16  [:mem 2]
  0xFE1B   i64.atomic.store8   [:mem 1]
  0xFE1C   i64.atomic.store16  [:mem 2]
  0xFE1D   i64.atomic.store32  [:mem 4]

  0xFE1E   i32.atomic.rmw.add     [:mem 4]
  0xFE1F   i64.atomic.rmw.add     [:mem 8]
  0xFE20   i32.atomic.rmw8.add_u  [:mem 1]
  0xFE21   i32.atomic.rmw16.add_u [:mem 2]
  0xFE22   i64.atomic.rmw8.add_u  [:mem 1]
  0xFE23   i64.atomic.rmw16.add_u [:mem 2]
  0xFE24   i64.atomic.rmw32.add_u [:mem 4]

  0xFE25   i32.atomic.rmw.sub     [:mem 4]
  0xFE26   i64.atomic.rmw.sub     [:mem 8]
  0xFE27   i32.atomic.rmw8.sub_u  [:mem 1]
  0xFE28   i32.atomic.rmw16.sub_u [:mem 2]
  0xFE29   i64.atomic.rmw8.sub_u  [:mem 1]
  0xFE2A   i64.atomic.rmw16.sub_u [:mem 2]
  0xFE2B   i64.atomic.rmw32.sub_u [:mem 4]

  0xFE2C   i32.atomic.rmw.and     [:mem 4]
  0xFE2D   i64.atomic.rmw.and     [:mem 8]
  0xFE2E   i32.atomic.rmw8.and_u  [:mem 1]
  0xFE2F   i32.atomic.rmw16.and_u [:mem 2]
  0xFE30   i64.atomic.rmw8.and_u  [:mem 1]
  0xFE31   i64.atomic.rmw16.and_u [:mem 2]
  0xFE32   i64.atomic.rmw32.and_u [:mem 4]

  0xFE33   i32.atomic.rmw.or     [:mem 4]
  0xFE34   i64.atomic.rmw.or     [:mem 8]
  0xFE35   i32.atomic.rmw8.or_u  [:mem 1]
  0xFE36   i32.atomic.rmw16.or_u [:mem 2]
  0xFE37   i64.atomic.rmw8.or_u  [:mem 1]
  0xFE38   i64.atomic.rmw16.or_u [:mem 2]
  0xFE39   i64.atomic.rmw32.or_u [:mem 4]

  0xFE3A   i32.atomic.rmw.xor     [:mem 4]
  0xFE3B   i64.atomic.rmw.xor     [:mem 8]
  0xFE3C   i32.atomic.rmw8.xor_u  [:mem 1]
  0xFE3D   i32.atomic.rmw16.xor_u [:mem 2]
  0xFE3E   i64.atomic.rmw8.xor_u  [:mem 1]
  0xFE3F   i64.atomic.rmw16.xor_u [:mem 2]
  0xFE40   i64.atomic.rmw32.xor_u [:mem 4]

  0xFE41   i32.atomic.rmw.xchg     [:mem 4]
  0xFE42   i64.atomic.rmw.xchg     [:mem 8]
  0xFE43   i32.atomic.rmw8.xchg_u  [:mem 1]
  0xFE44   i32.atomic.rmw16.xchg_u [:mem 2]
  0xFE45   i64.atomic.rmw8.xchg_u  [:mem 1]
  0xFE46   i64.atomic.rmw16.xchg_u [:mem 2]
  0xFE47   i64.atomic.rmw32.xchg_u [:mem 4]

  0xFE48   i32.atomic.rmw.cmpxchg     [:mem 4]
  0xFE49   i64.atomic.rmw.cmpxchg     [:mem 8]
  0xFE4A   i32.atomic.rmw8.cmpxchg_u  [:mem 1]
  0xFE4B   i32.atomic.rmw16.cmpxchg_u [:mem 2]
  0xFE4C   i64.atomic.rmw8.cmpxchg_u  [:mem 1]
  0xFE4D   i64.atomic.rmw16.cmpxchg_u [:mem 2]
  0xFE4E   i64.atomic.rmw32.cmpxchg_u [:mem 4]

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
