(ns waet.inst
  (:use [waet.util])
  (:require [waet.impl.scan :as scan]))

(def db
  (atom {:names {}      ; name -> opcode
         :opcodes {}})) ; opcode -> instruction

(defn scan-instruction []
  (let [opcode (scan/pred int?)
        name (scan/pred symbol?)
        immediates (scan/zom #(scan/pred keyword?))]
    {:opcode opcode
     :name name
     :immediates (vec immediates)}))

(defn scan-instructions []
  (let [docstring (scan/optional (scan/pred string?))]
    ;; TODO: Use the docstring for something?
    (scan/zom scan-instruction)))

(defn install-instructions [form]
  (doseq [{:keys [opcode name] :as instruction} (scan/from form (scan-instructions))]
    (when (get-in @db [:names name])
      (fail "redefining instruction" {:name name}))
    (when (get-in @db [:opcodes opcode])
      (fail "redefining opcode" {:opcode opcode}))
    (swap! db assoc-in [:names name] opcode)
    (swap! db update :opcodes assoc opcode instruction)))

(defmacro definstructions [description & body]
  `(install-instructions '~(with-meta body (meta &form))))

(definstructions "Control"

  0x00 unreachable
  0x01 nop

  0x02 block :block
  0x03 loop  :block
  0x04 if    :if
  0x05 else
  0x0B end

  0x0C br             :label
  0x0D br_if          :label
  0x0E br_table       :br_table
  0x0F return
  0x10 call           :call
  0x11 call_indirect  :call_indirect

  )

(definstructions "Parametric"

  0x1A drop
  0x1B select

  )

(definstructions "Variables"

  0x20 local.get   :local
  0x21 local.set   :local
  0x22 local.tee   :local
  0x23 global.get  :global
  0x24 global.set  :global

  )

(definstructions "Memory"

  0x28 i32.load       :mem4
  0x29 i64.load       :mem8
  0x2A f32.load       :mem4
  0x2B f64.load       :mem8
  0x2C i32.load8_s    :mem1
  0x2D i32.load8_u    :mem1
  0x2E i32.load16_s   :mem2
  0x2F i32.load16_u   :mem2
  0x30 i64.load8_s    :mem1
  0x31 i64.load8_u    :mem1
  0x32 i64.load16_s   :mem2
  0x33 i64.load16_u   :mem2
  0x34 i64.load32_s   :mem4
  0x35 i64.load32_u   :mem4
  0x36 i32.store      :mem4
  0x37 i64.store      :mem8
  0x38 f32.store      :mem4
  0x39 f64.store      :mem8
  0x3A i32.store8     :mem1
  0x3B i32.store16    :mem2
  0x3C i64.store8     :mem1
  0x3D i64.store16    :mem2
  0x3E i64.store32    :mem4
  0x3F memory.size
  0x40 memory.grow

  )

(definstructions "Numeric"

  0x41 i32.const  :i32
  0x42 i64.const  :i64
  0x43 f32.const  :f32
  0x44 f64.const  :f64

  0x45 i32.eqz
  0x46 i32.eq
  0x47 i32.ne
  0x48 i32.lt_s
  0x49 i32.lt_u
  0x4A i32.gt_s
  0x4B i32.gt_u
  0x4C i32.le_s
  0x4D i32.le_u
  0x4E i32.ge_s
  0x4F i32.ge_u

  0x50 i64.eqz
  0x51 i64.eq
  0x52 i64.ne
  0x53 i64.lt_s
  0x54 i64.lt_u
  0x55 i64.gt_s
  0x56 i64.gt_u
  0x57 i64.le_s
  0x58 i64.le_u
  0x59 i64.ge_s
  0x5A i64.ge_u

  0x5B f32.eq
  0x5C f32.ne
  0x5D f32.lt
  0x5E f32.gt
  0x5F f32.le
  0x60 f32.ge

  0x61 f64.eq
  0x62 f64.ne
  0x63 f64.lt
  0x64 f64.gt
  0x65 f64.le
  0x66 f64.ge

  0x67 i32.clz
  0x68 i32.ctz
  0x69 i32.popcnt
  0x6A i32.add
  0x6B i32.sub
  0x6C i32.mul
  0x6D i32.div_s
  0x6E i32.div_u
  0x6F i32.rem_s
  0x70 i32.rem_u
  0x71 i32.and
  0x72 i32.or
  0x73 i32.xor
  0x74 i32.shl
  0x75 i32.shr_s
  0x76 i32.shr_u
  0x77 i32.rotl
  0x78 i32.rotr

  0x79 i64.clz
  0x7A i64.ctz
  0x7B i64.popcnt
  0x7C i64.add
  0x7D i64.sub
  0x7E i64.mul
  0x7F i64.div_s
  0x80 i64.div_u
  0x81 i64.rem_s
  0x82 i64.rem_u
  0x83 i64.and
  0x84 i64.or
  0x85 i64.xor
  0x86 i64.shl
  0x87 i64.shr_s
  0x88 i64.shr_u
  0x89 i64.rotl
  0x8A i64.rotr

  0x8B f32.abs
  0x8C f32.neg
  0x8D f32.ceil
  0x8E f32.floor
  0x8F f32.trunc
  0x90 f32.nearest
  0x91 f32.sqrt
  0x92 f32.add
  0x93 f32.sub
  0x94 f32.mul
  0x95 f32.div
  0x96 f32.fmin
  0x97 f32.fmax
  0x98 f32.copysign

  0x99 f64.abs
  0x9A f64.neg
  0x9B f64.ceil
  0x9C f64.floor
  0x9D f64.trunc
  0x9E f64.nearest
  0x9F f64.sqrt
  0xA0 f64.add
  0xA1 f64.sub
  0xA2 f64.mul
  0xA3 f64.div
  0xA4 f64.fmin
  0xA5 f64.fmax
  0xA6 f64.copysign

  0xA7 i32.wrap/i64
  0xA8 i32.trunc_s/f32
  0xA9 i32.trunc_u/f32
  0xAA i32.trunc_s/f64
  0xAB i32.trunc_u/f64
  0xAC i64.extend_s/i32
  0xAD i64.extend_u/i32
  0xAE i64.trunc_s/f32
  0xAF i64.trunc_u/f32
  0xB0 i64.trunc_s/f64
  0xB1 i64.trunc_u/f64
  0xB2 f32.convert_s/i32
  0xB3 f32.convert_u/i32
  0xB4 f32.convert_s/i64
  0xB5 f32.convert_u/i64
  0xB6 f32.demote/f64
  0xB7 f64.convert_s/i32
  0xB8 f64.convert_u/i32
  0xB9 f64.convert_s/i64
  0xBA f64.convert_u/i64
  0xBB f64.promote/f32
  0xBC i32.reinterpret/f32
  0xBD i64.reinterpret/f64
  0xBE f32.reinterpret/i32
  0xBF f64.reinterpret/i64

  )


(definstructions "Threading"

  0xFE00  memory.atomic.notify :mem4
  0xFE01  memory.atomic.wait32 :mem4
  0xFE02  memory.atomic.wait64 :mem8

  0xFE0300  atomic.fence

  0xFE10   i32.atomic.load      :mem4
  0xFE11   i64.atomic.load      :mem8
  0xFE12   i32.atomic.load8_u   :mem1
  0xFE13   i32.atomic.load16_u  :mem2
  0xFE14   i64.atomic.load8_u   :mem1
  0xFE15   i64.atomic.load16_u  :mem2
  0xFE16   i64.atomic.load32_u  :mem4

  0xFE17   i32.atomic.store     :mem4
  0xFE18   i64.atomic.store     :mem8
  0xFE19   i32.atomic.store8    :mem1
  0xFE1A   i32.atomic.store16   :mem2
  0xFE1B   i64.atomic.store8    :mem1
  0xFE1C   i64.atomic.store16   :mem2
  0xFE1D   i64.atomic.store32   :mem4

  0xFE1E   i32.atomic.rmw.add      :mem4
  0xFE1F   i64.atomic.rmw.add      :mem8
  0xFE20   i32.atomic.rmw8.add_u   :mem1
  0xFE21   i32.atomic.rmw16.add_u  :mem2
  0xFE22   i64.atomic.rmw8.add_u   :mem1
  0xFE23   i64.atomic.rmw16.add_u  :mem2
  0xFE24   i64.atomic.rmw32.add_u  :mem4

  0xFE25   i32.atomic.rmw.sub      :mem4
  0xFE26   i64.atomic.rmw.sub      :mem8
  0xFE27   i32.atomic.rmw8.sub_u   :mem1
  0xFE28   i32.atomic.rmw16.sub_u  :mem2
  0xFE29   i64.atomic.rmw8.sub_u   :mem1
  0xFE2A   i64.atomic.rmw16.sub_u  :mem2
  0xFE2B   i64.atomic.rmw32.sub_u  :mem4

  0xFE2C   i32.atomic.rmw.and      :mem4
  0xFE2D   i64.atomic.rmw.and      :mem8
  0xFE2E   i32.atomic.rmw8.and_u   :mem1
  0xFE2F   i32.atomic.rmw16.and_u  :mem2
  0xFE30   i64.atomic.rmw8.and_u   :mem1
  0xFE31   i64.atomic.rmw16.and_u  :mem2
  0xFE32   i64.atomic.rmw32.and_u  :mem4

  0xFE33   i32.atomic.rmw.or      :mem4
  0xFE34   i64.atomic.rmw.or      :mem8
  0xFE35   i32.atomic.rmw8.or_u   :mem1
  0xFE36   i32.atomic.rmw16.or_u  :mem2
  0xFE37   i64.atomic.rmw8.or_u   :mem1
  0xFE38   i64.atomic.rmw16.or_u  :mem2
  0xFE39   i64.atomic.rmw32.or_u  :mem4

  0xFE3A   i32.atomic.rmw.xor      :mem4
  0xFE3B   i64.atomic.rmw.xor      :mem8
  0xFE3C   i32.atomic.rmw8.xor_u   :mem1
  0xFE3D   i32.atomic.rmw16.xor_u  :mem2
  0xFE3E   i64.atomic.rmw8.xor_u   :mem1
  0xFE3F   i64.atomic.rmw16.xor_u  :mem2
  0xFE40   i64.atomic.rmw32.xor_u  :mem4

  0xFE41   i32.atomic.rmw.xchg      :mem4
  0xFE42   i64.atomic.rmw.xchg      :mem8
  0xFE43   i32.atomic.rmw8.xchg_u   :mem1
  0xFE44   i32.atomic.rmw16.xchg_u  :mem2
  0xFE45   i64.atomic.rmw8.xchg_u   :mem1
  0xFE46   i64.atomic.rmw16.xchg_u  :mem2
  0xFE47   i64.atomic.rmw32.xchg_u  :mem4

  0xFE48   i32.atomic.rmw.cmpxchg      :mem4
  0xFE49   i64.atomic.rmw.cmpxchg      :mem8
  0xFE4A   i32.atomic.rmw8.cmpxchg_u   :mem1
  0xFE4B   i32.atomic.rmw16.cmpxchg_u  :mem2
  0xFE4C   i64.atomic.rmw8.cmpxchg_u   :mem1
  0xFE4D   i64.atomic.rmw16.cmpxchg_u  :mem2
  0xFE4E   i64.atomic.rmw32.cmpxchg_u  :mem4

  )

(definstructions "Exception Handling"

  0x1F  try        :block
  0x08  throw      :tag
  0x0a  throw_ref

  )

(definstructions "SIMD"

  ;;; Memory instructions.
  0xFD00  v128.load          :mem16
  0xFD01  v128.load8x8_s     :mem16
  0xFD02  v128.load8x8_u     :mem16
  0xFD03  v128.load16x4_s    :mem16
  0xFD04  v128.load16x4_u    :mem16
  0xFD05  v128.load32x2_s    :mem16
  0xFD06  v128.load32x2_u    :mem16
  0xFD07  v128.load8_splat   :mem16
  0xFD08  v128.load16_splat  :mem16
  0xFD09  v128.load32_splat  :mem16
  0xFD0A  v128.load64_splat  :mem16
  0xFD0B  v128.store         :mem16

  ;;; Basic operations.
  0xFD0C  v128.const      :v128
  0xFD0D  i8x16.shuffle   :lane32x16
  0xFD0E  i8x16.swizzle

  ;;; Splat operations.
  0xFD0F  i8x16.splat
  0xFD10  i16x8.splat
  0xFD11  i32x4.splat
  0xFD12  i64x2.splat
  0xFD13  f32x4.splat
  0xFD14  f64x2.splat

  ;;; Lane operations.
  0xFD15  i8x16.extract_lane_s  :lane16
  0xFD16  i8x16.extract_lane_u  :lane16
  0xFD17  i8x16.replace_lane    :lane16
  0xFD18  i16x8.extract_lane_s  :lane8
  0xFD19  i16x8.extract_lane_u  :lane8
  0xFD1A  i16x8.replace_lane    :lane8
  0xFD1B  i32x4.extract_lane    :lane4
  0xFD1C  i32x4.replace_lane    :lane4
  0xFD1D  i64x2.extract_lane    :lane2
  0xFD1E  i64x2.replace_lane    :lane2
  0xFD1F  f32x4.extract_lane    :lane4
  0xFD20  f32x4.replace_lane    :lane4
  0xFD21  f64x2.extract_lane    :lane2
  0xFD22  f64x2.replace_lane    :lane2

   0xFD23  i8x16.eq        0xFD2D  i16x8.eq         0xFD37  i32x4.eq
   0xFD24  i8x16.ne        0xFD2E  i16x8.ne         0xFD38  i32x4.ne
   0xFD25  i8x16.lt_s      0xFD2F  i16x8.lt_s       0xFD39  i32x4.lt_s
   0xFD26  i8x16.lt_u      0xFD30  i16x8.lt_u       0xFD3A  i32x4.lt_u
   0xFD27  i8x16.gt_s      0xFD31  i16x8.gt_s       0xFD3B  i32x4.gt_s
   0xFD28  i8x16.gt_u      0xFD32  i16x8.gt_u       0xFD3C  i32x4.gt_u
   0xFD29  i8x16.le_s      0xFD33  i16x8.le_s       0xFD3D  i32x4.le_s
   0xFD2A  i8x16.le_u      0xFD34  i16x8.le_u       0xFD3E  i32x4.le_u
   0xFD2B  i8x16.ge_s      0xFD35  i16x8.ge_s       0xFD3F  i32x4.ge_s
   0xFD2C  i8x16.ge_u      0xFD36  i16x8.ge_u       0xFD40  i32x4.ge_u

   0xFD41  f32x4.eq        0xFD47  f64x2.eq
   0xFD42  f32x4.ne        0xFD48  f64x2.ne
   0xFD43  f32x4.lt        0xFD49  f64x2.lt
   0xFD44  f32x4.gt        0xFD4A  f64x2.gt
   0xFD45  f32x4.le        0xFD4B  f64x2.le
   0xFD46  f32x4.ge        0xFD4C  f64x2.ge

   0xFD4D  v128.not
   0xFD4E  v128.and
   0xFD4F  v128.andnot
   0xFD50  v128.or
   0xFD51  v128.xor
   0xFD52  v128.bitselect
   0xFD53  v128.any_true

   ;;; Load Lane Operations.
   0xFD54  v128.load8_lane    :mem8  :lane16
   0xFD55  v128.load16_lane   :mem16 :lane8
   0xFD56  v128.load32_lane   :mem32 :lane4
   0xFD57  v128.load64_lane   :mem64 :lane2
   0xFD58  v128.store8_lane   :mem8  :lane16
   0xFD59  v128.store16_lane  :mem16 :lane8
   0xFD5A  v128.store32_lane  :mem32 :lane4
   0xFD5B  v128.store64_lane  :mem64 :lane2
   0xFD5C  v128.load32_zero   :mem4
   0xFD5D  v128.load64_zero   :mem8

  ;;; Float conversion.
  0xFD5E  f32x4.demote_f64x2_zero
  0xFD5F  f64x2.promote_low_f32x4

  0xFD60   i8x16.abs                          0xFD80  i16x8.abs                        0xFDA0  i32x4.abs                      0xFDC0  i64x2.abs
  0xFD61   i8x16.neg                          0xFD81  i16x8.neg                        0xFDA1  i32x4.neg                      0xFDC1  i64x2.neg
  0xFD62   i8x16.popcnt                       0xFD82  i16x8.q15mulr_sat_s
  0xFD63   i8x16.all_true                     0xFD83  i16x8.all_true                   0xFDA3  i32x4.all_true                 0xFDC3  i64x2.all_true
  0xFD64   i8x16.bitmask                      0xFD84  i16x8.bitmask                    0xFDA4  i32x4.bitmask                  0xFDC4  i64x2.bitmask
  0xFD65   i8x16.narrow_i16x8_s               0xFD85  i16x8.narrow_i32x4_s
  0xFD66   i8x16.narrow_i16x8_u               0xFD86  i16x8.narrow_i32x4_u
  0xFD67   f32x4.ceil                         0xFD87  i16x8.extend_low_i8x16_s         0xFDA7  i32x4.extend_low_i16x8_s       0xFDC7  i64x2.extend_low_i32x4_s
  0xFD68   f32x4.floor                        0xFD88  i16x8.extend_high_i8x16_s        0xFDA8  i32x4.extend_high_i16x8_s      0xFDC8  i64x2.extend_high_i32x4_s
  0xFD69   f32x4.trunc                        0xFD89  i16x8.extend_low_i8x16_u         0xFDA9  i32x4.extend_low_i16x8_u       0xFDC9  i64x2.extend_low_i32x4_u
  0xFD6A   f32x4.nearest                      0xFD8A  i16x8.extend_high_i8x16_u        0xFDAA  i32x4.extend_high_i16x8_u      0xFDCA  i64x2.extend_high_i32x4_u
  0xFD6B   i8x16.shl                          0xFD8B  i16x8.shl                        0xFDAB  i32x4.shl                      0xFDCB  i64x2.shl
  0xFD6C   i8x16.shr_s                        0xFD8C  i16x8.shr_s                      0xFDAC  i32x4.shr_s                    0xFDCC  i64x2.shr_s
  0xFD6D   i8x16.shr_u                        0xFD8D  i16x8.shr_u                      0xFDAD  i32x4.shr_u                    0xFDCD  i64x2.shr_u
  0xFD6E   i8x16.add                          0xFD8E  i16x8.add                        0xFDAE  i32x4.add                      0xFDCE  i64x2.add
  0xFD6F   i8x16.add_sat_s                    0xFD8F  i16x8.add_sat_s
  0xFD70   i8x16.add_sat_u                    0xFD90  i16x8.add_sat_u
  0xFD71   i8x16.sub                          0xFD91  i16x8.sub                        0xFDB1  i32x4.sub                      0xFDD1  i64x2.sub
  0xFD72   i8x16.sub_sat_s                    0xFD92  i16x8.sub_sat_s
  0xFD73   i8x16.sub_sat_u                    0xFD93  i16x8.sub_sat_u
  0xFD74   f64x2.ceil                         0xFD94  f64x2.nearest
  0xFD75   f64x2.floor                        0xFD95  i16x8.mul                        0xFDB5  i32x4.mul                      0xFDD5  i64x2.mul
  0xFD76   i8x16.min_s                        0xFD96  i16x8.min_s                      0xFDB6  i32x4.min_s                    0xFDD6  i64x2.eq
  0xFD77   i8x16.min_u                        0xFD97  i16x8.min_u                      0xFDB7  i32x4.min_u                    0xFDD7  i64x2.ne
  0xFD78   i8x16.max_s                        0xFD98  i16x8.max_s                      0xFDB8  i32x4.max_s                    0xFDD8  i64x2.lt_s
  0xFD79   i8x16.max_u                        0xFD99  i16x8.max_u                      0xFDB9  i32x4.max_u                    0xFDD9  i64x2.gt_s
  0xFD7A   f64x2.trunc                                                                 0xFDBA  i32x4.dot_i16x8_s              0xFDDA  i64x2.le_s
  0xFD7B   i8x16.avgr_u                       0xFD9B  i16x8.avgr_u                                                            0xFDDB  i64x2.ge_s
  0xFD7C   i16x8.extadd_pairwise_i8x16_s      0xFD9C  i16x8.extmul_low_i8x16_s         0xFDBC  i32x4.extmul_low_i16x8_s       0xFDDC  i64x2.extmul_low_i32x4_s
  0xFD7D   i16x8.extadd_pairwise_i8x16_u      0xFD9D  i16x8.extmul_high_i8x16_s        0xFDBD  i32x4.extmul_high_i16x8_s      0xFDDD  i64x2.extmul_high_i32x4_s
  0xFD7E   i32x4.extadd_pairwise_i16x8_s      0xFD9E  i16x8.extmul_low_i8x16_u         0xFDBE  i32x4.extmul_low_i16x8_u       0xFDDE  i64x2.extmul_low_i32x4_u
  0xFD7F   i32x4.extadd_pairwise_i16x8_u      0xFD9F  i16x8.extmul_high_i8x16_u        0xFDBF  i32x4.extmul_high_i16x8_u      0xFDDF  i64x2.extmul_high_i32x4_u

  0xFDE0  f32x4.abs         0xFDEC  f64x2.abs
  0xFDE1  f32x4.neg         0xFDED  f64x2.neg
  0xFDE3  f32x4.sqrt        0xFDEF  f64x2.sqrt
  0xFDE4  f32x4.add         0xFDF0  f64x2.add
  0xFDE5  f32x4.sub         0xFDF1  f64x2.sub
  0xFDE6  f32x4.mul         0xFDF2  f64x2.mul
  0xFDE7  f32x4.div         0xFDF3  f64x2.div
  0xFDE8  f32x4.min         0xFDF4  f64x2.min
  0xFDE9  f32x4.max         0xFDF5  f64x2.max
  0xFDEA  f32x4.pmin        0xFDF6  f64x2.pmin
  0xFDEB  f32x4.pmax        0xFDF7  f64x2.pmax

  ;;; Conversion operations.
  0xFDF8  i32x4.trunc_sat_f32x4_s
  0xFDF9  i32x4.trunc_sat_f32x4_u
  0xFDFA  f32x4.convert_i32x4_s
  0xFDFB  f32x4.convert_i32x4_u
  0xFDFC  i32x4.trunc_sat_f64x2_s_zero
  0xFDFD  i32x4.trunc_sat_f64x2_u_zero
  0xFDFE  f64x2.convert_low_i32x4_s
  0xFDFF  f64x2.convert_low_i32x4_u

  )

(def by-opcode
  (:opcodes @db))

(def operations
  (-> by-opcode vals vec))

(def by-name
  (into {} (map (juxt :name identity)) (vals by-opcode)))
