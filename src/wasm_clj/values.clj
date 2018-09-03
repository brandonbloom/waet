(ns wasm-clj.values)

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
       (<= 0 x))) ;TODO: check max.

(defn u64? [x]
  (and (int? x)
       (<= 0 x))) ;TODO: check max.

(defn i32? [x]
  (int? x)) ;TODO: check min/max

(defn i64? [x]
  (int? x)) ;TODO: check min/max

(defn f32? [x]
  (float? x)) ;TODO: check min/max

(defn f64? [x]
  (float? x)) ;TODO: check min/max

(def index? u32?)
