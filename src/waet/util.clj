(ns waet.util)

(defn fail
  ([msg] (fail msg {}))
  ([msg data]
   (throw (ex-info msg data))))

(defmacro change! [var f & args]
  `(set! ~var (~f ~var ~@args)))

(defn tap [x f]
  (f x)
  x)
