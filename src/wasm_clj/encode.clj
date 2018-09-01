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

(defn leb128-write-loop [shift-right until n]
  (loop [^long n n]
    (let [b (bit-and n 0x7F)
          n (shift-right n 7)]
      (if (until n b)
        (.write *w* b)
        (do (.write *w* (bit-or b 0x80))
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

(defn begin-section [id]
  (.write *w* ^byte id)
  (reserve-leb128))

(defn end-section [offset]
  ;XXX write size at offset
  )

(defmacro writing-section [id & body]
  `(let [offset# :XXX ;XXX
         _# (begin-section ~id)
         res# (do ~@body)]
     (end-section offset#)
     res#))

(defn write-vec [write xs]
  (write-unsigned-leb128 (count xs))
  (run! write xs))

(defn write-char [char]
  (.write *w* (byte char))) ;XXX aux utf8 function

(defn write-name [name]
  (write-vec write-char (str name)))

(defn write-importdesc [{:keys [head] :as desc}]
  (case head
    func (do (.write *w* 0x00)
             (write-unsigned-leb128 (:typeidx desc))) ;XXX wrong key?
    ;XXX table
    ;XXX mem
    ;XXX global
    ))

(defn write-import [{:keys [name module desc] :as import}]
  (write-name name)
  (write-name module)
  (write-importdesc desc))

(defn write-importsec [imports]
  (writing-section 0x02
    (write-vec write-import imports)))

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
