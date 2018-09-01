(ns wasm-clj.encode
  (:use [wasm-clj.util])
  (:import [java.io ByteArrayOutputStream]))

(def ^:dynamic ^ByteArrayOutputStream *w*)

(defn write-magic []
  (doto *w*
    (.write 0x00)
    (.write 0x61)
    (.write 0x73)
    (.write 0x6D)))

(defn write-version []
  (doto *w*
    (.write 0x01)
    (.write 0x00)
    (.write 0x00)
    (.write 0x00)))

(defn reserve-leb128 []
  (dotimes [_ 5]
    (.write *w* 0x00)))

(defn write-leb128 [n]
  (.write *w* 0x00)) ;XXX write leb-128

(defn begin-section [id]
  (.write *w* id)
  (reserve-leb128))

(defn end-section [offset]
  ;XXX write size at offset
  )

(defmacro writing-section [id & body]
  `(let [offset# :XXX
         _# (begin-section ~id)
         res# (do ~@body)] ;XXX
     (end-section offset#)
     res#))

(defn write-vec [write xs]
  (write-leb128 (count xs))
  (run! write xs))

(defn write-char [char]
  (.write *w* (byte char))) ;XXX aux utf8 function

(defn write-name [name]
  (write-vec write-char (str name)))

(defn write-importdesc [{:keys [head] :as desc}]
  (case head
    func (do (.write *w* 0x00)
             (write-leb128 (:typeidx desc))) ;XXX wrong key?
    ; table
    ; mem
    ; global
    ))

(defn write-import [{:keys [name module desc] :as import}]
  (write-name name)
  (write-name module)
  (write-importdesc desc))

(defn write-importsec [imports]
  (writing-section 0x02
    (write-vec import imports)))

(defn write-module [ast]
  (write-magic)
  (write-version)
  ;TODO: custom sections interleaved throughout.
  (write-importsec (:imports ast))
  ;(write-typesec (:types ast))
  ;(write-tablesec (:tables ast))
  ;(write-globalsec (:globals ast))
  ;(write-exportsec (:exports ast))
  ;(write-startsec (:start ast))
  ;(write-elemsec (:elems ast))
  ;(write-codesec (:code ast))
  ;(write-datasec (:data ast))
  )

(comment

  (require '[wasm-clj.analyze3 :as analyze])
  (binding [*w* (ByteArrayOutputStream.)]
    (let [form '(module
                  (func (export "the_answer")
                    i32.const 42))
          ast (analyze/module form)
          _ (fipp.edn/pprint ast)
          _ (write-module ast)
          bytes (.toByteArray *w*)]
      (prn (seq bytes))))

)
