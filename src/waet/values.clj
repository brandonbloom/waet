(ns waet.values
  (:require [waet.util :refer :all]))

(defn name? [x]
  ;;TODO: Tighter validation.
  (string? x))

(defn id? [x]
  (and (simple-symbol? x)
       (= (first (str x)) \$)))

(defn value? [x]
  (or (integer? x)
      (float? x)
      (string? x) ; Includes names.
      (id? x)))

(defn u8?  [x] (and (int? x) (<= 0 x))) ;TODO: check max.
(defn u16? [x] (and (int? x) (<= 0 x))) ;TODO: check max.
(defn u32? [x] (and (int? x) (<= 0 x))) ;TODO: check max.
(defn u64? [x] (and (int? x) (<= 0 x))) ;TODO: check max.

(defn i8?  [x] (and (int? x) (<=    Byte/MIN_VALUE  x     Byte/MAX_VALUE)))
(defn i16? [x] (and (int? x) (<=   Short/MIN_VALUE  x    Short/MAX_VALUE)))
(defn i32? [x] (and (int? x) (<= Integer/MIN_VALUE  x  Integer/MAX_VALUE)))
(defn i64? [x] (and (int? x) (<=    Long/MIN_VALUE  x     Long/MAX_VALUE)))

(defn f32? [x] (number? x)) ;TODO: check min/max
(defn f64? [x] (number? x)) ;TODO: check min/max

(defn v128? [x] (bigint? x)) ;TODO: check min/max

(def index? u32?)

(defrecord Annotation [head tail])
