(ns waet.values)

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

(defn u32? [x]
  (and (integer? x)
       (<= 0 x))) ;TODO: check max.

(defn u64? [x]
  (and (integer? x)
       (<= 0 x))) ;TODO: check max.

(defn i32? [x]
  (integer? x)) ;TODO: check min/max

(defn i64? [x]
  (integer? x)) ;TODO: check min/max

(defn f32? [x]
  (number? x)) ;TODO: check min/max

(defn f64? [x]
  (number? x)) ;TODO: check min/max

(def index? u32?)

(defrecord Annotation [head tail])
