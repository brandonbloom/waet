(ns wasm-clj.parse
  (:use [wasm-clj.util])
  (:require [wasm-clj.values :refer [id? u32? index? name?]]))

;;;; See https://webassembly.github.io/spec/core/text/index.html

(defn bad-syntax [form message]
  (fail (str "syntax error: " message)
        {:form form}))

;;; Utils.

(defn check-phrase [form]
  (when-not (seq? form)
    (bad-syntax form "expected list"))
  (when-not (simple-symbol? (first form))
    (bad-syntax form "expected symbol at head of list")))

(defn phrase? [form]
  (and (seq? form)
       (simple-symbol? (first form))))

(defn has-head? [head form]
  (and (phrase? form)
       (= (first form) head)))

;;; Fields.

(def ^:dynamic *module*)

(defn fresh-id [name]
  (let [n (:counter *module*)]
    (change! *module* update :counter inc)
    (symbol (str "$" name "__" n)))) ;TODO: Something less likely to collide.

(defn emit-field [section field]
  (let [field (assoc field :sort :modulefield)
        index (-> field section count)]
    (change! *module* update :fields conj [section index])
    (when-let [id (:id field)]
      (change! *module* assoc-in [section :env id] index))
    (change! *module* update-in [section :fields] conj field)
    index))

;;; Scanning.

(def ^:dynamic *input*)

(defmacro scanning [input & body]
  `(binding [*input* ~input]
     (let [res# ~@body]
       (when-first [x# (seq *input*)]
         (bad-syntax x# "expected end of sequence"))
       res#)))

(defn scan-rest []
  (let [xs *input*]
    (set! *input* nil)
    (vec xs)))

(def no-match-exception (Exception. "no match"))

(defn no-match []
  ;; Uncomment exception allocation to get stacktraces for debugging.
  (throw (ex-info "no match" {::no-match true}))
  (throw no-match-exception))

(defn no-match? [ex]
  (or (identical? ex no-match-exception) ; Fast path (preallocated).
      (::no-match (ex-data ex))))        ; Slow path (with stacktrace).

(defmacro scan-opt [& body]
  `(try
     ~@body
     (catch Exception ex#
       (when-not (no-match? ex#)
         (throw ex#)))))

(defn scan-pred [pred]
  (let [[x & xs] *input*]
    (if (pred x)
      (do (set! *input* xs)
          x)
      (no-match))))

(defn scan-all [scan]
  (loop [v []]
    (if-let [x (scan-opt (scan))]
      (recur (conj v x))
      v)))

(defn scan-u32 []
  (scan-pred u32?))

(defn scan-id []
  (scan-pred id?))

(defn scan-name []
  (scan-pred name?))

(defn scan-phrase
  ([]
   (scan-pred phrase?))
  ([head]
   (scan-pred #(has-head? head %))))

(defn scan-import []
  (scan-phrase 'import))

(defn scan-typeuse []
  (let [type-form (scan-opt (scan-phrase 'type))
        param-forms (scan-all #(scan-phrase 'param))
        result-forms (scan-all #(scan-phrase 'result))
        forms (concat (when type-form
                        [type-form])
                      param-forms
                      result-forms)
        id (when type-form
             (scanning (next type-form)
               (scan-id)))
        params (mapv second param-forms) ;TODO: validate.
        results (mapv second result-forms) ;TODO: validate.
        signature [params results]
        type {:id id
              :params params
              :results results}
        index (or (get-in *module* [:signatures signature])
                  (let [index (emit-field :types type)]
                    (change! *module* assoc-in [:signatures signature] index)
                    index))]
    (assoc type
           :index index
           :forms forms)))

(defn scan-limits []
  (let [n (scan-u32)
        m (scan-opt (scan-u32))]
    {:min n
     :max (or m n)}))

(defn scan-memtype []
  (scan-limits))

(defn scan-elemtype []
  (scan-pred #{'anyfunc}))

(defn scan-tabletype []
  (let [limits (scan-limits)
        elemtype (scan-elemtype)]
    {:limits limits
     :elemtype elemtype}))

(defn scan-valtype []
  (scan-pred '#{i32 i64 f32 f64}))

(defn scan-globaltype []
  (let [var (scan-opt (scan-phrase 'mut))]
    (if-let [[head & tail] var]
      (scanning tail
        (let [type (scan-valtype)]
          {:head head
           :kind :var
           :type type}))
      (let [type (scan-valtype)]
        {:kind :const
         :type type}))))

(defn scan-inline-export []
  (let [form (scan-phrase 'export)]
    (scanning (next form)
      (let [name (scan-name)]
        {:head 'export
         :form form
         :name name}))))

(defn scan-inline-import []
  (let [form (scan-phrase 'import)]
    (scanning (next form)
      (let [module (scan-name)
            name (scan-name)]
        {:head 'import
         :form form
         :module module
         :name name}))))

(defn scan-locals []
  (scan-all #(scan-phrase 'local)))

;;; Module Fields.

(defmulti -parse-modulefield first)

(defn parse-modulefield [form]
  (check-phrase form)
  (-parse-modulefield form))

(defmethod -parse-modulefield 'type [ast]
  (fail "cannot parse/-modulefield 'type"))

(defn scan-importdesc []
  (let [[head & tail :as form] (scan-phrase)
        ast (scanning tail
              (let [id (scan-opt (scan-id))
                    [section type] (case head
                                     func [:funcs (scan-typeuse)]
                                     table [:tables (scan-tabletype)]
                                     memory [:mems (scan-memtype)]
                                     global [:globals (scan-globaltype)]
                                     (fail (str "cannot import " head)))]
                {:id id
                 :section section
                 :type type}))]
    (assoc ast :head head :form form)))

(defmethod -parse-modulefield 'import [[head & tail :as form]]
  (scanning tail
    (let [module (scan-name)
          name (scan-name)
          desc (scan-importdesc)
          ast {:head head
               :form form
               :module module
               :name name
               :desc desc}]
      (emit-field :imports ast))))

(defmethod -parse-modulefield 'func [[head & tail :as form]]
  (scanning tail
    (let [id (scan-opt (scan-id))
          import (scan-opt (scan-inline-import))]
      (if-let [{:keys [module name]} import]
        (let [type (scan-typeuse)]
          (parse-modulefield (list 'import module name
                                   (concat ['func]
                                           (when id [id])
                                           (:forms type)))))
        (if-let [{:keys [name]} (scan-opt (scan-inline-export))]
          (let [tail (scan-rest)
                id (fresh-id name)]
            (parse-modulefield (list 'export name (list 'func id)))
            (parse-modulefield (list* 'func id tail)))
          (let [type (scan-typeuse)
                locals (scan-locals)
                body (scan-rest)
                func {:head 'func
                      :form form
                      :id id
                      :type type
                      :locals locals
                      :body body}]
            (emit-field :funcs func)))))))

(defmethod -parse-modulefield 'table [ast]
  (fail "cannot parse/-modulefield 'table"))

(defmethod -parse-modulefield 'mem [ast]
  (fail "cannot parse/-modulefield 'mem"))

(defmethod -parse-modulefield 'global [ast]
  (fail "cannot parse/-modulefield 'global"))

(defn scan-exportdesc []
  (let [[head & tail :as form] (scan-phrase)
        section (case head
                  func :funcs
                  table :tables
                  memory :mems
                  global :globals
                  (fail (str "cannot export " head)))]
    (scanning tail
      (let [id (scan-id)]
        {:sort :exportdesc
         :head head
         :form form
         :section section
         :id id}))))

(defmethod -parse-modulefield 'export [[head & tail :as form]]
  (scanning tail
    (let [name (scan-name)
          desc (scan-exportdesc)
          ast {:head head
               :form form
               :name name
               :desc desc}]
      (emit-field :exports ast))))

(defmethod -parse-modulefield 'start [ast]
  (fail "cannot parse/-modulefield 'start"))

(defmethod -parse-modulefield 'elem [ast]
  (fail "cannot parse/-modulefield 'elem"))

(defmethod -parse-modulefield 'data [ast]
  (fail "cannot parse/-modulefield 'data"))

;;; Modules.

(def empty-section {:env {} :fields []})

(defn parse-module [[head & tail :as form]]
  {:pre [(has-head? 'module form)]}
  (binding [*module* {:sort :toplevel
                      :head head
                      :counter 0
                      :signatures {}
                      :fields empty-section
                      :types empty-section
                      :funcs empty-section
                      :imports empty-section
                      :exports empty-section}]
    (run! parse-modulefield tail)
    *module*))

;;; End.

(comment

  (defmacro party [form]
    (require 'fipp.edn)
    `(fipp.edn/pprint (parse-module '~form)))

  (party (module
            (func $i (import "imports" "imported_func") (param i32))
            (func (export "exported_func")
                  i32.const 42
                  call $i)))

)
