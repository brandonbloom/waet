(ns wasm-clj.io
  (:require [clojure.core :as clj])
  (:import [java.io RandomAccessFile Closeable]))

(defprotocol WriteSeeker
  (^long position [self])
  (seek [self pos])
  (write-byte [self, value])
  (write-bytes [self, ^bytes value]))

(deftype FileWriter [^RandomAccessFile f]
  WriteSeeker
  (position [self]
    (.. f getChannel position))
  (seek [self pos]
    (.seek f pos))
  (write-byte [self value]
    (.writeByte f value))
  (write-bytes [self value]
    (.write f ^bytes value))
  Closeable
  (close [self]
    (.close f)))

(defn open-file-writer [^String path]
  (let [f (RandomAccessFile. path "rw")]
    (.setLength f 0)
    (FileWriter. f)))
