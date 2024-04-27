(ns waet.inst)

(def header [:opcode :name :immediates])
(def table '[

  ;;; Control Instructions.

  0x00 unreachable    :none
  0x01 nop            :none

  0x02 block          :block
  0x03 loop           :block
  0x04 if             :if
  0x05 else           :none
  0x0B end            :none

  0x0C br             :label
  0x0D br_if          :label
  0x0E br_table       :br_table
  0x0F return         :none
  0x10 call           :call
  0x11 call_indirect  :call_indirect

  ;;; Parametric Instructions.

  0x1A drop    :none
  0x1B select  :none

  ;;; Variable Instructions.

  0x20 local.get   :local
  0x21 local.set   :local
  0x22 local.tee   :local
  0x23 global.get  :global
  0x24 global.set  :global

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
  0x3F memory.size    :none
  0x40 memory.grow    :none

  ;;; Numeric Instructions.

  0x41 i32.const  :i32
  0x42 i64.const  :i64
  0x43 f32.const  :f32
  0x44 f64.const  :f64

  0x45 i32.eqz   :none
  0x46 i32.eq    :none
  0x47 i32.ne    :none
  0x48 i32.lt_s  :none
  0x49 i32.lt_u  :none
  0x4A i32.gt_s  :none
  0x4B i32.gt_u  :none
  0x4C i32.le_s  :none
  0x4D i32.le_u  :none
  0x4E i32.ge_s  :none
  0x4F i32.ge_u  :none

  0x50 i64.eqz   :none
  0x51 i64.eq    :none
  0x52 i64.ne    :none
  0x53 i64.lt_s  :none
  0x54 i64.lt_u  :none
  0x55 i64.gt_s  :none
  0x56 i64.gt_u  :none
  0x57 i64.le_s  :none
  0x58 i64.le_u  :none
  0x59 i64.ge_s  :none
  0x5A i64.ge_u  :none

  0x5B f32.eq  :none
  0x5C f32.ne  :none
  0x5D f32.lt  :none
  0x5E f32.gt  :none
  0x5F f32.le  :none
  0x60 f32.ge  :none

  0x61 f64.eq  :none
  0x62 f64.ne  :none
  0x63 f64.lt  :none
  0x64 f64.gt  :none
  0x65 f64.le  :none
  0x66 f64.ge  :none

  0x67 i32.clz     :none
  0x68 i32.ctz     :none
  0x69 i32.popcnt  :none
  0x6A i32.add     :none
  0x6B i32.sub     :none
  0x6C i32.mul     :none
  0x6D i32.div_s   :none
  0x6E i32.div_u   :none
  0x6F i32.rem_s   :none
  0x70 i32.rem_u   :none
  0x71 i32.and     :none
  0x72 i32.or      :none
  0x73 i32.xor     :none
  0x74 i32.shl     :none
  0x75 i32.shr_s   :none
  0x76 i32.shr_u   :none
  0x77 i32.rotl    :none
  0x78 i32.rotr    :none

  0x79 i64.clz     :none
  0x7A i64.ctz     :none
  0x7B i64.popcnt  :none
  0x7C i64.add     :none
  0x7D i64.sub     :none
  0x7E i64.mul     :none
  0x7F i64.div_s   :none
  0x80 i64.div_u   :none
  0x81 i64.rem_s   :none
  0x82 i64.rem_u   :none
  0x83 i64.and     :none
  0x84 i64.or      :none
  0x85 i64.xor     :none
  0x86 i64.shl     :none
  0x87 i64.shr_s   :none
  0x88 i64.shr_u   :none
  0x89 i64.rotl    :none
  0x8A i64.rotr    :none

  0x8B f32.abs       :none
  0x8C f32.neg       :none
  0x8D f32.ceil      :none
  0x8E f32.floor     :none
  0x8F f32.trunc     :none
  0x90 f32.nearest   :none
  0x91 f32.sqrt      :none
  0x92 f32.add       :none
  0x93 f32.sub       :none
  0x94 f32.mul       :none
  0x95 f32.div       :none
  0x96 f32.fmin      :none
  0x97 f32.fmax      :none
  0x98 f32.copysign  :none

  0x99 f64.abs       :none
  0x9A f64.neg       :none
  0x9B f64.ceil      :none
  0x9C f64.floor     :none
  0x9D f64.trunc     :none
  0x9E f64.nearest   :none
  0x9F f64.sqrt      :none
  0xA0 f64.add       :none
  0xA1 f64.sub       :none
  0xA2 f64.mul       :none
  0xA3 f64.div       :none
  0xA4 f64.fmin      :none
  0xA5 f64.fmax      :none
  0xA6 f64.copysign  :none

  0xA7 i32.wrap/i64         :none
  0xA8 i32.trunc_s/f32      :none
  0xA9 i32.trunc_u/f32      :none
  0xAA i32.trunc_s/f64      :none
  0xAB i32.trunc_u/f64      :none
  0xAC i64.extend_s/i32     :none
  0xAD i64.extend_u/i32     :none
  0xAE i64.trunc_s/f32      :none
  0xAF i64.trunc_u/f32      :none
  0xB0 i64.trunc_s/f64      :none
  0xB1 i64.trunc_u/f64      :none
  0xB2 f32.convert_s/i32    :none
  0xB3 f32.convert_u/i32    :none
  0xB4 f32.convert_s/i64    :none
  0xB5 f32.convert_u/i64    :none
  0xB6 f32.demote/f64       :none
  0xB7 f64.convert_s/i32    :none
  0xB8 f64.convert_u/i32    :none
  0xB9 f64.convert_s/i64    :none
  0xBA f64.convert_u/i64    :none
  0xBB f64.promote/f32      :none
  0xBC i32.reinterpret/f32  :none
  0xBD i64.reinterpret/f64  :none
  0xBE f32.reinterpret/i32  :none
  0xBF f64.reinterpret/i64  :none

  ;;; Threading.

  0xFE00 memory.atomic.notify [:mem 4]
  0xFE01 memory.atomic.wait32 [:mem 4]
  0xFE02 memory.atomic.wait64 [:mem 8]

  0xFE0300 atomic.fence :none

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

  ;;; Exception handling.

  0x1F  try        :block
  0x08  throw      :tag
  0x0a  throw_ref  :none

  ;;;; SIMD.

  ;;; Memory instructions.
  0x00    v128.load          [:mem 16]
  0x01    v128.load8x8_s     [:mem 16]
  0x02    v128.load8x8_u     [:mem 16]
  0x03    v128.load16x4_s    [:mem 16]
  0x04    v128.load16x4_u    [:mem 16]
  0x05    v128.load32x2_s    [:mem 16]
  0x06    v128.load32x2_u    [:mem 16]
  0x07    v128.load8_splat   [:mem 16]
  0x08    v128.load16_splat  [:mem 16]
  0x09    v128.load32_splat  [:mem 16]
  0x0a    v128.load64_splat  [:mem 16]
  0x0b    v128.store         [:mem 16]

  ;;; Basic operations.
  0x0c    v128.const      :v128
  ;0x0d    i8x16.shuffle
  0x0e    i8x16.swizzle   :none

   ;;; Splat operations.
   0x0f   i8x16.splat  :none
   0x10   i16x8.splat  :none
   0x11   i32x4.splat  :none
   0x12   i64x2.splat  :none
   0x13   f32x4.splat  :none
   0x14   f64x2.splat  :none

  ;;; Lane operations.
  ; 0x15  i8x16.extract_lane_s i:ImmLaneIdx16
  ; 0x16  i8x16.extract_lane_u  .... https://github.com/WebAssembly/spec/blob/main/proposals/simd/BinarySIMD.md
  ; 0x17  i8x16.replace_lane
  ; 0x18  i16x8.extract_lane_s
  ; 0x19  i16x8.extract_lane_u
  ; 0x1a  i16x8.replace_lane
  ; 0x1b  i32x4.extract_lane
  ; 0x1c  i32x4.replace_lane
  ; 0x1d  i64x2.extract_lane
  ; 0x1e  i64x2.replace_lane
  ; 0x1f  f32x4.extract_lane
  ; 0x20  f32x4.replace_lane
  ; 0x21  f64x2.extract_lane
  ; 0x22  f64x2.replace_lane

   0x23  i8x16.eq    :none      0x2d  i16x8.eq     :none         0x37  i32x4.eq    :none
   0x24  i8x16.ne    :none      0x2e  i16x8.ne     :none         0x38  i32x4.ne    :none
   0x25  i8x16.lt_s  :none      0x2f  i16x8.lt_s   :none         0x39  i32x4.lt_s  :none
   0x26  i8x16.lt_u  :none      0x30  i16x8.lt_u   :none         0x3a  i32x4.lt_u  :none
   0x27  i8x16.gt_s  :none      0x31  i16x8.gt_s   :none         0x3b  i32x4.gt_s  :none
   0x28  i8x16.gt_u  :none      0x32  i16x8.gt_u   :none         0x3c  i32x4.gt_u  :none
   0x29  i8x16.le_s  :none      0x33  i16x8.le_s   :none         0x3d  i32x4.le_s  :none
   0x2a  i8x16.le_u  :none      0x34  i16x8.le_u   :none         0x3e  i32x4.le_u  :none
   0x2b  i8x16.ge_s  :none      0x35  i16x8.ge_s   :none         0x3f  i32x4.ge_s  :none
   0x2c  i8x16.ge_u  :none      0x36  i16x8.ge_u   :none         0x40  i32x4.ge_u  :none

   0x41  f32x4.eq  :none        0x47  f64x2.eq  :none
   0x42  f32x4.ne  :none        0x48  f64x2.ne  :none
   0x43  f32x4.lt  :none        0x49  f64x2.lt  :none
   0x44  f32x4.gt  :none        0x4a  f64x2.gt  :none
   0x45  f32x4.le  :none        0x4b  f64x2.le  :none
   0x46  f32x4.ge  :none        0x4c  f64x2.ge  :none

   0x4d  v128.not        :none
   0x4e  v128.and        :none
   0x4f  v128.andnot     :none
   0x50  v128.or         :none
   0x51  v128.xor        :none
   0x52  v128.bitselect  :none
   0x53  v128.any_true   :none

   ;;; Load Lane Operations.
   ; TODO: immediates
   ; 0x54  v128.load8_lane
   ; 0x55  v128.load16_lane
   ; 0x56  v128.load32_lane
   ; 0x57  v128.load64_lane
   ; 0x58  v128.store8_lane
   ; 0x59  v128.store16_lane
   ; 0x5a  v128.store32_lane
   ; 0x5b  v128.store64_lane
   0x5c  v128.load32_zero  [:mem 4]
   0x5d  v128.load64_zero  [:mem 8]

  ;;; Float conversion.
  0x5e  f32x4.demote_f64x2_zero  :none
  0x5f  f64x2.promote_low_f32x4  :none

  0x60   i8x16.abs                      :none         0x80  i16x8.abs                  :none         0xa0  i32x4.abs                  :none       0xc0  i64x2.abs                 :none
  0x61   i8x16.neg                      :none         0x81  i16x8.neg                  :none         0xa1  i32x4.neg                  :none       0xc1  i64x2.neg                 :none
  0x62   i8x16.popcnt                   :none         0x82  i16x8.q15mulr_sat_s        :none         0xa2                             :none
  0x63   i8x16.all_true                 :none         0x83  i16x8.all_true             :none         0xa3  i32x4.all_true             :none       0xc3  i64x2.all_true            :none
  0x64   i8x16.bitmask                  :none         0x84  i16x8.bitmask              :none         0xa4  i32x4.bitmask              :none       0xc4  i64x2.bitmask             :none
  0x65   i8x16.narrow_i16x8_s           :none         0x85  i16x8.narrow_i32x4_s       :none
  0x66   i8x16.narrow_i16x8_u           :none         0x86  i16x8.narrow_i32x4_u       :none
  0x67   f32x4.ceil                     :none         0x87  i16x8.extend_low_i8x16_s   :none         0xa7  i32x4.extend_low_i16x8_s   :none       0xc7  i64x2.extend_low_i32x4_s  :none
  0x68   f32x4.floor                    :none         0x88  i16x8.extend_high_i8x16_s  :none         0xa8  i32x4.extend_high_i16x8_s  :none       0xc8  i64x2.extend_high_i32x4_s :none
  0x69   f32x4.trunc                    :none         0x89  i16x8.extend_low_i8x16_u   :none         0xa9  i32x4.extend_low_i16x8_u   :none       0xc9  i64x2.extend_low_i32x4_u  :none
  0x6a   f32x4.nearest                  :none         0x8a  i16x8.extend_high_i8x16_u  :none         0xaa  i32x4.extend_high_i16x8_u  :none       0xca  i64x2.extend_high_i32x4_u :none
  0x6b   i8x16.shl                      :none         0x8b  i16x8.shl                  :none         0xab  i32x4.shl                  :none       0xcb  i64x2.shl                 :none
  0x6c   i8x16.shr_s                    :none         0x8c  i16x8.shr_s                :none         0xac  i32x4.shr_s                :none       0xcc  i64x2.shr_s               :none
  0x6d   i8x16.shr_u                    :none         0x8d  i16x8.shr_u                :none         0xad  i32x4.shr_u                :none       0xcd  i64x2.shr_u               :none
  0x6e   i8x16.add                      :none         0x8e  i16x8.add                  :none         0xae  i32x4.add                  :none       0xce  i64x2.add                 :none
  0x6f   i8x16.add_sat_s                :none         0x8f  i16x8.add_sat_s            :none
  0x70   i8x16.add_sat_u                :none         0x90  i16x8.add_sat_u            :none
  0x71   i8x16.sub                      :none         0x91  i16x8.sub                  :none         0xb1  i32x4.sub                  :none       0xd1  i64x2.sub                 :none
  0x72   i8x16.sub_sat_s                :none         0x92  i16x8.sub_sat_s            :none
  0x73   i8x16.sub_sat_u                :none         0x93  i16x8.sub_sat_u            :none
  0x74   f64x2.ceil                     :none         0x94  f64x2.nearest              :none
  0x75   f64x2.floor                    :none         0x95  i16x8.mul                  :none         0xb5  i32x4.mul                  :none       0xd5  i64x2.mul                 :none
  0x76   i8x16.min_s                    :none         0x96  i16x8.min_s                :none         0xb6  i32x4.min_s                :none       0xd6  i64x2.eq                  :none
  0x77   i8x16.min_u                    :none         0x97  i16x8.min_u                :none         0xb7  i32x4.min_u                :none       0xd7  i64x2.ne                  :none
  0x78   i8x16.max_s                    :none         0x98  i16x8.max_s                :none         0xb8  i32x4.max_s                :none       0xd8  i64x2.lt_s                :none
  0x79   i8x16.max_u                    :none         0x99  i16x8.max_u                :none         0xb9  i32x4.max_u                :none       0xd9  i64x2.gt_s                :none
  0x7a   f64x2.trunc                    :none         0x9a                             :none         0xba  i32x4.dot_i16x8_s          :none       0xda  i64x2.le_s                :none
  0x7b   i8x16.avgr_u                   :none         0x9b  i16x8.avgr_u               :none                                                      0xdb  i64x2.ge_s                :none
  0x7c   i16x8.extadd_pairwise_i8x16_s  :none         0x9c  i16x8.extmul_low_i8x16_s   :none         0xbc  i32x4.extmul_low_i16x8_s   :none       0xdc  i64x2.extmul_low_i32x4_s  :none
  0x7d   i16x8.extadd_pairwise_i8x16_u  :none         0x9d  i16x8.extmul_high_i8x16_s  :none         0xbd  i32x4.extmul_high_i16x8_s  :none       0xdd  i64x2.extmul_high_i32x4_s :none
  0x7e   i32x4.extadd_pairwise_i16x8_s  :none         0x9e  i16x8.extmul_low_i8x16_u   :none         0xbe  i32x4.extmul_low_i16x8_u   :none       0xde  i64x2.extmul_low_i32x4_u  :none
  0x7f   i32x4.extadd_pairwise_i16x8_u  :none         0x9f  i16x8.extmul_high_i8x16_u  :none         0xbf  i32x4.extmul_high_i16x8_u  :none       0xdf  i64x2.extmul_high_i32x4_u :none

  0xe0  f32x4.abs   :none        0xec  f64x2.abs   :none
  0xe1  f32x4.neg   :none        0xed  f64x2.neg   :none
  0xe3  f32x4.sqrt  :none        0xef  f64x2.sqrt  :none
  0xe4  f32x4.add   :none        0xf0  f64x2.add   :none
  0xe5  f32x4.sub   :none        0xf1  f64x2.sub   :none
  0xe6  f32x4.mul   :none        0xf2  f64x2.mul   :none
  0xe7  f32x4.div   :none        0xf3  f64x2.div   :none
  0xe8  f32x4.min   :none        0xf4  f64x2.min   :none
  0xe9  f32x4.max   :none        0xf5  f64x2.max   :none
  0xea  f32x4.pmin  :none        0xf6  f64x2.pmin  :none
  0xeb  f32x4.pmax  :none        0xf7  f64x2.pmax  :none

  ;;; Conversion operations.
  0xf8  i32x4.trunc_sat_f32x4_s       :none
  0xf9  i32x4.trunc_sat_f32x4_u       :none
  0xfa  f32x4.convert_i32x4_s         :none
  0xfb  f32x4.convert_i32x4_u         :none
  0xfc  i32x4.trunc_sat_f64x2_s_zero  :none
  0xfd  i32x4.trunc_sat_f64x2_u_zero  :none
  0xfe  f64x2.convert_low_i32x4_s     :none
  0xff  f64x2.convert_low_i32x4_u     :none

])

(defn parse-row [row]
  (let [{:keys [immediates] :as operation} (into {} (mapv vector header row))]
    (if (vector? immediates)
      (assoc operation :immediates (first immediates) :align (last immediates))
      operation)))

(def operations
  (->> (partition (count header) table)
       (mapv parse-row)))

(def by-name
  (into {} (map (juxt :name identity)) operations))
