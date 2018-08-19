(ns wasm-clj.util)

(defn fail
  ([msg] (fail msg {}))
  ([msg data]
   (throw (ex-info msg data))))
