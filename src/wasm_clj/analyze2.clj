(ns wasm-clj.analyze2
  (:require [clojure.spec.alpha :as s])
  (:use [wasm-clj.util]))

;;;; See https://webassembly.github.io/spec/core/text/index.html

(defmacro phrase-spec [head & body-specs]
  `(s/spec (s/and seq?
                  (s/cat :head #{'~head} ~@body-specs))))

;;; Values
;;; https://webassembly.github.io/spec/core/text/values.html

(defn name? [x]
  ;;TODO: Tighter validation.
  (string? x))

(defn id? [x]
  (and (simple-symbol? x)
       (= (first (str x)) \$)))

(defn value? [x]
  (or (int? x)
      (float? x)
      (string? x) ; Includes names.
      (id? x)))

(defn u32? [x]
  (and (int? x)
       (<= 0 x))) ;TODO: max.

(defn u64? [x]
  (and (int? x)
       (<= 0 x))) ;TODO: max.

;;; Types
;;; https://webassembly.github.io/spec/core/text/types.html

(def valtypes '#{i32 i64 f32 f64})

(s/def ::valtype
  valtypes)

(s/def ::resulttype
  (s/? ::result))

(s/def ::functype
  (phrase-spec func
    :params (s/* ::param)
    :results (s/* ::result)))

(s/def ::param
  (s/alt :full (phrase-spec param
                 :id (s/? id?)
                 :valtype ::valtype)
         :abbreviated (s/* (phrase-spec param
                             :valtypes (s/* ::valtype)))))

(s/def ::result
  (s/alt :full (phrase-spec result
                 :valtype ::valtype)
         :abbreviated (phrase-spec result
                        :valtypes (s/* :valtype))))

(s/def ::limits
  (s/cat :value u32?
         :range (s/cat :min u32? :max u32?)))

(s/def ::memtype
  (s/cat :limits ::limits))

(s/def ::tabletype
  (s/cat :limits ::limits
         :elemtype ::elemtype))

(s/def ::elemtype
  #{"anyfunc"})

(s/def ::globaltype
  (s/alt :const ::typeval
         :var ::typeval))

;;; Instructions
;;; https://webassembly.github.io/spec/core/text/instructions.html

(s/def ::instr
  (s/alt :plain ::plaininstr
         :block ::blockinstr))

(s/def ::label
  (s/? id?))

(s/def ::blockinstr
  (s/alt

    ;; Control Instructions (block).
    :block (phrase-spec block
             :label ::label
             :resulttype ::resulttype
             :instructions (s/* ::instr)
             :end #{'end}
             :id (s/? id?))
    :loop (phrase-spec loop
            :label ::label
            :resulttype ::resulttype
            :instructions (s/* ::instr)
            :end #{'end}
            :id (s/? id?))
    :if (phrase-spec if
          :label ::label
          :resulttype ::resulttype
          :then (s/cat :instructions (s/* ::instr))
          :else (s/? (s/cat :else #{'else}
                            :instructions (s/* ::instr)))
          :end #{'end})

    ))

(s/def ::plaininstr
  (s/alt

    ;; Control Instructions (plain).
    :unreachable (s/cat :head #{'unreachable})
    :nop (s/cat :head #{'nop})
    :br (s/cat :head #{'br}
               :label ::labelidx)
    :br_if (s/cat :head #{'br_if}
                  :label ::labelidx)
    :br_table (s/cat :head #{'br_table}
                     :table (s/* ::labelidx)
                     :else ::labelidx)
    :return (s/cat :head #{'return})
    :call (s/cat :head #{'call}
                 :funcidx ::funcidx)
    :call_indirect (s/cat :head #{'call_indirect}
                          :typeuse ::typeuse)

    ;; Parametric Instructions.
    :drop (s/cat :head #{'drop})
    :select (s/cat :head #{'select})

    ;; Variable Instructions.
    :get_local (s/cat :head #{'get_local}
                      :local ::localidx)
    :set_local (s/cat :head #{'set_local}
                      :local ::localidx)
    :tee_local (s/cat :head #{'tee_local}
                      :local ::localidx)
    :get_global (s/cat :head #{'get_global}
                       :global ::localidx)
    :set_global (s/cat :head #{'get_global}
                       :global ::localidx)

    ;; Memory Instructions.

    :memop (s/cat :head #{(symbol "i32.load")
                          (symbol "i64.load")
                          (symbol "f32.load")
                          (symbol "f64.load")
                          (symbol "i32.load8_s")
                          (symbol "i32.load8_u")
                          (symbol "i32.load16_s")
                          (symbol "i32.load16_u")
                          (symbol "i64.load8_s")
                          (symbol "i64.load8_u")
                          (symbol "i64.load16_s")
                          (symbol "i64.load16_u")
                          (symbol "i64.load32_s")
                          (symbol "i64.load32_u")
                          (symbol "i32.store")
                          (symbol "i64.store")
                          (symbol "f32.store")
                          (symbol "f64.store")
                          (symbol "i32.store8")
                          (symbol "i32.store16")
                          (symbol "i64.store8")
                          (symbol "i64.store16")
                          (symbol "i64.store32")}
                  :args ::memarg)
    :memory.size (s/cat :head #{(symbol "memory.size")})
    :memory.grow (s/cat :head #{(symbol "memory.grow")})


    ;; Numeric Instructions.

    :iconst (s/cat :head '#{(symbol "i32.const")
                            (symbol "i64.const")}
                   :value int?)
    :fconst (s/cat :head '#{(symbol "f32.const")
                            (symbol "f64.const")}
                   :value float?)

    :numop (s/cat :head '#{

                           (symbol "i32.clz")
                           (symbol "i32.ctz")
                           (symbol "i32.popcnt")
                           (symbol "i32.add")
                           (symbol "i32.sub")
                           (symbol "i32.mul")
                           (symbol "i32.div_s")
                           (symbol "i32.div_u")
                           (symbol "i32.rem_s")
                           (symbol "i32.rem_u")
                           (symbol "i32.and")
                           (symbol "i32.or")
                           (symbol "i32.xor")
                           (symbol "i32.shl")
                           (symbol "i32.shr_s")
                           (symbol "i32.shr_u")
                           (symbol "i32.rotl")
                           (symbol "i32.rotr")

                           (symbol "i64.clz")
                           (symbol "i64.ctz")
                           (symbol "i64.popcnt")
                           (symbol "i64.add")
                           (symbol "i64.sub")
                           (symbol "i64.mul")
                           (symbol "i64.div_s")
                           (symbol "i64.div_u")
                           (symbol "i64.rem_s")
                           (symbol "i64.rem_u")
                           (symbol "i64.and")
                           (symbol "i64.or")
                           (symbol "i64.xor")
                           (symbol "i64.shl")
                           (symbol "i64.shr_s")
                           (symbol "i64.shr_u")
                           (symbol "i64.rotl")
                           (symbol "i64.rotr")

                           (symbol "f32.abs")
                           (symbol "f32.neg")
                           (symbol "f32.ceil")
                           (symbol "f32.floor")
                           (symbol "f32.trunc")
                           (symbol "f32.nearest")
                           (symbol "f32.sqrt")
                           (symbol "f32.add")
                           (symbol "f32.sub")
                           (symbol "f32.mul")
                           (symbol "f32.div")
                           (symbol "f32.min")
                           (symbol "f32.max")
                           (symbol "f32.copysign")

                           (symbol "f64.abs")
                           (symbol "f64.neg")
                           (symbol "f64.ceil")
                           (symbol "f64.floor")
                           (symbol "f64.trunc")
                           (symbol "f64.nearest")
                           (symbol "f64.sqrt")
                           (symbol "f64.add")
                           (symbol "f64.sub")
                           (symbol "f64.mul")
                           (symbol "f64.div")
                           (symbol "f64.min")
                           (symbol "f64.max")
                           (symbol "f64.copysign")

                           ;;XXX resume at i32.eqz

             })

    ))

(s/def ::memarg
  (s/cat :offset ::offset
         :align ::align))

(s/def ::offset
  (s/? (s/cat :head #{'offset=}
              :offset u32?)))

(s/def ::align
  (s/? (s/cat :head #{'align=}
              :align u32?)))

;;TODO: Folded Instructions.

;;TODO: Expressions.


;;;TODO: Modules

(s/def ::import-abbrev
  (phrase-spec import
    :module name?
    :name name?))

(s/def ::export-abbrev
  (s/? (phrase-spec export
                    :name name?)))

;; Indicies.

(s/def ::typeidx u32?)
(s/def ::funcidx u32?)
(s/def ::tableidx u32?)
(s/def ::memidx u32?)
(s/def ::globalidx u32?)
(s/def ::localidx u32?)
(s/def ::labelidx u32?)

;; Types.

(s/def ::type
  (phrase-spec type
    :id (s/? id?)
    :functype ::functype))

;; Type Uses.

(s/def ::typeuse
  (s/cat :type (s/? (phrase-spec type
                      :typeidx ::typeidx))
         :params (s/* ::param)
         :results (s/* ::result)))

;; Imports.

(s/def ::import
  (phrase-spec import
    :module name?
    :name name?
    :desc ::importdesc))

(s/def ::importdesc
  (s/alt :func (phrase-spec func
                 :id (s/? id?)
                 :func ::typeuse)
         :table (phrase-spec table
                  :id (s/? id?)
                  :table ::tabletype)
         :memory (phrase-spec memory
                   :id (s/? id?)
                   :mem ::memtype)
         :global (phrase-spec global
                   :id (s/? id?)
                   :global ::globaltype)))

;; Functions.

(s/def ::func
  (s/alt :import (phrase-spec func
                   :id (s/? id?)
                   :import ::import-abbrev
                   :type ::typeuse)
         :define (phrase-spec func
                   :id (s/? id?)
                   :export ::export-abbrev
                   :type ::typeuse
                   :locals (s/* ::local)
                   :body (s/* ::instr))))

(s/def ::local
  (s/alt :named (phrase-spec local
                  :id (s/? id?)
                  :type ::valtype)
         :anon (phrase-spec local
                 :types (s/* ::valtype))))

;; Tables.

(s/def ::table
  (s/alt :import (phrase-spec table
                   :id (s/? id?)
                   :import ::import-abbrev
                   :type ::tabletype)
         :define (phrase-spec table
                   :id (s/? id?)
                   :export ::export-abbrev
                   :type ::tabletype
                   :elem (s/? ::elem))))

;; Memories.

(s/def ::mem
  (s/alt :import (phrase-spec memory
                   :id (s/? id?)
                   :import ::import-abbrev
                   :type ::memtype)
         :define (phrase-spec memory
                   :id (s/? id?)
                   :export ::export-abbrev
                   :memory (s/alt :type ::memtype
                                  :data ::data))))

;; Globals.

(s/def ::global
  (s/alt :import (phrase-spec global
                   :id (s/? id?)
                   :import ::import-abbrev
                   :type ::globaltype)
         :define (phrase-spec global
                   :id (s/? id?)
                   :export ::export-abbrev
                   :type ::globaltype)))

;; Exports.

(s/def ::export
  (phrase-spec export
    :name name?
    :desc ::exportdesc))

(s/def ::exportdesc
  (s/alt :func (phrase-spec func
                 :func ::funcidx)
         :table (phrase-spec table
                  :table :tableidx)
         :memory (phrase-spec memory
                  :memory :memoryidx)
         :global (phrase-spec global
                  :global :globalidx)))

;; Start Function.

(s/def ::start
  (phrase-spec start
    :func ::funcidx))

;; Element Segments.

(s/def ::elem
  (phrase-spec elem
    :table (s/? ::tableidx)
    :offset ::fieldoffset
    :init (s/* ::funcidx)))

(s/def ::fieldoffset
  (s/alt :instr ::instr
         :expr (phrase-spec offset
                 :offset ::expr)))

;; Data Segments.

(s/def ::data
  (phrase-spec data
    :data (s/? ::memidx)
    :offset ::fieldoffset
    :init (s/* ::datastring)))

(s/def ::datastring
  string?)

;; Modules.

(s/def ::module
  (phrase-spec module
    :id (s/? id?)
    :modulefields (s/* ::modulefield)))

(s/def ::sourcefile
  (s/* ::modulefield))

(s/def ::modulefield
  (s/alt :type   ::type
         :import ::import
         :func   ::func
         :table  ::table
         :mem    ::mem
         :global ::global
         :export ::export
         :start  ::start
         :elem   ::elem
         :data   ::data))


(comment

  (defmacro party [form]
    `(fipp.edn/pprint (s/conform ::module '~form)))

  (false? (id? 'x))
  (true? (id? '$x))

  ;;TODO Test more [MDN Examples](https://github.com/mdn/webassembly-examples).

  (party
    (module
      ;(func $i (import "imports" "imported_func") (param i32))
      ;(func (export "exported_func")
      ;  i32.const 42
      ;  call $i)
      ))

)
