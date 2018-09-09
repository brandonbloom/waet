(ns wabt-clj.io
  (:require [clojure.core :as clj]
            [clojure.edn :as edn])
  (:import [java.io RandomAccessFile Closeable]
           [java.util Arrays]
           [java.nio.charset Charset StandardCharsets]))


(defn ubyte [value]
  (if (>= value 128)
    (byte (- value 256))
    (byte value)))


(defn string-reader
  ([s]
   (string-reader s "UTF-8"))
  ([^String s, ^String encoding]
   (-> (.getBytes s encoding)
       java.io.ByteArrayInputStream.
       java.io.InputStreamReader.)))

(defn edn-seq [in]
  (cond
    (string? in) (recur (string-reader in))
    (sequential? in) (seq in)
    :else (let [r (java.io.PushbackReader. in)]
            (->> (repeatedly #(edn/read {:eof ::eof} r))
                 (take-while #(not= % ::eof))))))


(defprotocol IWriteSeeker
  (^long position [self])
  (seek [self pos])
  (write-byte [self, value])
  (write-bytes [self, ^bytes value]))

(deftype FileWriter [^RandomAccessFile f]
  IWriteSeeker
  (position [self]
    (.. f getChannel position))
  (seek [self pos]
    (.seek f pos))
  (write-byte [self value]
    (.writeByte f (ubyte value)))
  (write-bytes [self value]
    (.write f ^bytes value))
  Closeable
  (close [self]
    (.close f)))

(defn open-file-writer [^String path]
  (let [f (RandomAccessFile. path "rw")]
    (.setLength f 0)
    (FileWriter. f)))


(defn -ensure-size [^bytes bytes, ^long size]
  (if (<= size (alength bytes))
    bytes
    (Arrays/copyOf bytes (* size 2))))

(defprotocol IArrayWriter
  (array-writer-bytes [self]))

(deftype ArrayWriter [^:unsynchronized-mutable ^long pos,
                      ^:unsynchronized-mutable ^long end,
                      ^:unsynchronized-mutable ^bytes bs]
  IWriteSeeker
  (position [self]
    pos)
  (seek [self pos]
    (locking self
      (set! (.pos self) pos)))
  (write-byte [self value]
    (locking self
      (let [size (inc pos)]
        (set! bs (-ensure-size bs size))
        (aset bs pos ^byte (ubyte value))
        (set! pos size)
        (set! end (max pos end)))))
  (write-bytes [self value]
    (locking self
      (let [n (alength ^bytes value)
            size (+ pos n)]
        (set! bs (-ensure-size bs size))
        (System/arraycopy value 0 bs pos n)
        (set! pos size)
        (set! end (max pos end)))))
  Closeable
  (close [self]
    (set! bs nil)
    nil)
  IArrayWriter
  (array-writer-bytes [self]
    (Arrays/copyOf bs end)))

(defn new-array-writer []
  (ArrayWriter. 0 0 (byte-array 1024)))


(def ^Charset utf8 StandardCharsets/UTF_8)

(defn utf-8-bytes [^String s]
  (.getBytes s utf8))
