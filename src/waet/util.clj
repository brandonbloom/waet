(ns waet.util
  (:require [fipp.edn]))

(defn fail
  ([msg] (fail msg {}))
  ([msg data]
   (throw (ex-info msg data))))

(defmacro change! [var f & args]
  `(set! ~var (~f ~var ~@args)))

(defn tap [x f]
  (f x)
  x)

(defn bigint? [x]
  (instance? clojure.lang.BigInt x))

(def pprint fipp.edn/pprint)

(defn pprint-str [x]
  (with-out-str
    (pprint x)))

(defn make-log [& message]
  (->> message
       (map #(if (string? %)
               %
               (pprint-str %)))
       (apply str)))

(defn log [& message]
  (binding [*out* *err*]
    (println (apply make-log message))))

(defn fatal [& message]
  (apply log message)
  (System/exit 1))
