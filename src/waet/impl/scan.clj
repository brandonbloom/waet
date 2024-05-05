(ns waet.impl.scan
  "Mini parsing library for scanning EDN forms"
  (:use [waet.util]))

(def ^:dynamic *pos* nil)

(defn origin [x]
  (let [m (select-keys (meta x) [:line :column])]
    (when (seq m)
      m)))

(defn set-pos-from [x]
  (when-let [pos (origin x)]
    (set! *pos* pos)))

(defn bad-syntax
  ([form message]
   (bad-syntax form message nil))
  ([form message data]
   (fail (str "syntax error: " message)
         (merge {::error :bad-syntax
                 :form form}
                *pos*
                data))))

(def ^:dynamic *input*)

(def ^:dynamic *expected*)

(defmacro from [input & body]
  `(binding [*input* ~input
             *pos* (or (origin *input*) *pos*)
             *expected* nil]
     (let [res# (do ~@body)]
       (when-first [x# (seq *input*)]
         (if *expected*
           (bad-syntax x# (str "expected " *expected*))
           (bad-syntax x# "unexpected")))
       res#)))

(defn tail []
  (let [xs *input*]
    (set! *input* nil)
    (vec xs)))

(def no-match-exception (Exception. "no match"))

(def debug-no-match? true)

(defn no-match [expected]
  (set! *expected* expected)
  (when debug-no-match?
    (throw (ex-info "no match" {::error :no-match
                                :form (first *input*)
                                :expected expected
                                :near *pos*})))
  (throw no-match-exception))

(defn no-match? [ex]
  (or (identical? ex no-match-exception)      ; Fast path (preallocated).
      (-> ex ex-data ::error (= :no-match)))) ; Slow path (with stacktrace).

(defmacro optional [& body]
  `(try
     ~@body
     (catch Exception ex#
       (when-not (no-match? ex#)
         (throw ex#)))))

(defn pred* [label f]
  (let [[x & xs] *input*]
    (if (f x)
      (do (set! *input* xs)
          (set-pos-from x)
          x)
      (no-match label))))

(defmacro pred [f]
  `(pred* ~(pr-str f) ~f))

(defn one []
  (pred* "form" (constantly true)))

(defn zom [scan]
  (loop [v []]
    (if-let [x (optional (scan))]
      (recur (conj v x))
      v)))
