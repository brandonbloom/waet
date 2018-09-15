(ns wabt-clj.parse
  (:use [wabt-clj.util])
  (:require [wabt-clj.values :as val]
            [wabt-clj.inst :as inst]
            [wabt-clj.io :as io]))

;;;; See <https://webassembly.github.io/spec/core/text/index.html>.

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

(defn bind! [section id index]
  (let [path [section :env id]]
    (when (get-in *module* path)
      (fail (str "Redefinition of " id " in " section)))
    (change! *module* assoc-in path index)
    index))

(defn emit-field [section field]
  (let [index (-> *module* section :fields count)
        field (assoc field
                     :sort :modulefield
                     :index index)]
    (change! *module* update :fields conj [section :fields index])
    (when-let [id (:id field)]
      (bind! section id index))
    (change! *module* update-in [section :fields] conj field)
    index))

;;; Scanning.

(def ^:dynamic *input*)

(defmacro scanning [input & body]
  `(binding [*input* ~input]
     (let [res# (do ~@body)]
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
  (throw (ex-info "no match" {::no-match true
                              :form (first *input*)}))
  (throw no-match-exception))

(defn no-match? [ex]
  (or (identical? ex no-match-exception) ; Fast path (preallocated).
      (::no-match (ex-data ex))))        ; Slow path (with stacktrace).

(defmacro scanning-opt [& body]
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

(defn scan []
  (scan-pred (constantly true)))

(defn scan-all [scan]
  (loop [v []]
    (if-let [x (scanning-opt (scan))]
      (recur (conj v x))
      v)))

(defn scan-u32 []
  (scan-pred val/u32?))

(defn scan-id []
  (scan-pred val/id?))

(defn scan-index []
  ;; Either symbolic or numeric index.
  (scan-pred #(or (val/id? %) (val/index? %))))

(defn scan-name []
  (scan-pred val/name?))

(defn scan-phrase
  ([]
   (scan-pred phrase?))
  ([head]
   (scan-pred #(has-head? head %))))

(defn scan-import []
  (scan-phrase 'import))

(defn scan-typeid []
  (let [[_ & tail :as form] (scan-phrase 'type)]
    (scanning tail
      (let [id (scan-id)]
        {:head 'type
         :id id}))))

(defn scan-valtype []
  (scan-pred '#{i32 i64 f32 f64}))

(defn scan-local []
  (let [[_ & tail :as form] (scan-phrase 'local)]
    (scanning tail
      (if-let [id (scanning-opt (scan-id))]
        (let [type (scan-valtype)]
          {:head 'local
           :id id
           :type type
           :form form})
        (let [types (scan-all scan-valtype)]
          {:head 'local
           :types types
           :form form})))))

(defn scan-locals []
  (scan-all scan-local))

(defn scan-param []
  (let [[_ & tail :as form] (scan-phrase 'param)]
    (scanning tail
      (let [type (scan-valtype)]
        {:head 'param
         :type type
         :form form}))))

(defn scan-params []
  (scan-all scan-param))

(defn scan-result []
  (let [[_ & tail :as form] (scan-phrase 'result)]
    (scanning tail
      (let [type (scan-valtype)]
        {:head 'param
         :type type
         :form form}))))

(defn scan-results []
  (scan-all scan-result))

(defn scan-typeuse []
  (let [typeid (scanning-opt (scan-typeid))
        params (scan-params)
        results (scan-results)
        forms (concat (when typeid
                        [(:form typeid)])
                      (map :form params)
                      (map :form results))
        signature [(mapv :type params)
                   (mapv :type results)]
        type {:id (:id typeid)
              :typeid typeid
              :params params
              :results results}
        index (or (get-in *module* [:signatures signature])
                  (let [index (emit-field :types type)]
                    (change! *module* assoc-in [:signatures signature] index)
                    index))]
    (assoc type
           :index index
           :forms (vec forms))))

(defn scan-limits []
  (let [n (scan-u32)
        m (scanning-opt (scan-u32))]
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

(defn scan-globaltype []
  (let [var (scanning-opt (scan-phrase 'mut))]
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

;;; Instructions.

(def op? symbol?)

(declare scan-inst unfold)

(defn scan-body []
  (loop [body []]
    (let [{:keys [op] :as inst} (scan-inst)]
      (if ('#{end else} op)
        [body op]
        (recur (into body (unfold inst)))))))

(defn scan-label []
  (when-let [id (scanning-opt (scan-index))]
    {:id id}))

(defn scan-block []
  (let [label (scan-label)
        results (scan-results)
        [body terminator] (scan-body)
        id (scanning-opt (scan-id))]
    {:label label
     :results results
     :body body
     :id id
     :terminator terminator}))

(defn scan-end-body []
  (let [[body terminator] (scan-body)]
    (when (not= terminator 'end)
      (fail "expected end"))
    body))

(defn scan-then+else []
  (let [block (scan-block)
        blocks (-> block
                   (assoc :then (:body block))
                   (dissoc :body :terminator))
        else (case (:terminator block)
               end []
               else (scan-end-body))]
    (assoc blocks :else else)))

(defn scan-branches []
  (let [labels (scan-all scan-label)]
    {:branches (vec (butlast labels))
     :default (last labels)}))

(defn scan-kwarg [key scan-arg default]
  (let [value (if (scanning-opt (scan-pred #{(symbol (str key "="))}))
                (scan-arg)
                default)]
    {key value}))

(defn scan-memarg []
  (merge (scan-kwarg :offset scan-u32 0)
         (scan-kwarg :align scan-u32 0)))

(defn scan-op []
  (scan-pred op?))

(defn scan-inst* []
  (let [op (scan-op)
        args (case (get-in inst/by-name [op :shape])
               :nullary {}
               :block (scan-block)
               :if (scan-then+else)
               :label {:label (scan-label)}
               :br_table (scan-branches)
               :call {:func {:id (scan-index) :section :funcs}}
               :call_indirect {:type (scan-typeuse)}
               :local {:local {:id (scan-index)}}
               :global {:global {:id (scan-id) :section :globals}}
               :mem (scan-memarg)
               :i32 {:value (scan-pred val/i32?)}
               :i64 {:value (scan-pred val/i64?)}
               :f32 {:value (scan-pred val/f32?)}
               :f64 {:value (scan-pred val/f64?)}
               (fail (str "Can't scan " op)))]
    (assoc args :op op)))

(defn scan-inst []
  (if-let [[op & _ :as form] (scanning-opt (scan-phrase))]
    {:form form}
    (scan-inst*)))

(defn scan-expr []
  (vec (mapcat unfold (scan-all scan-inst))))

(defn parse-body [[head & tail :as form]]
  (scanning tail
    (let [label (scan-label)
          results (scan-results)
          body (scan-expr)]
      {:op head
       :id nil
       :label label
       :results results
       :body body
       :form form})))

(defn unfold-block [form]
  [(parse-body form)])

(defn unfold-if [form]
  (scanning form
    (scan #{'if})
    (let [label (scan-label)
          results (scan-results)
          then (-> (scan-phrase 'then)
                   parse-body)
          else (-> (scanning-opt (scan-phrase 'else))
                   (or (list 'else))
                   parse-body)]
      [{:op 'if
        :id nil
        :label label
        :results results
        :then then
        :else else
        :form form}])))

(defn unfold-plain [{:keys [form] :as inst}]
  (scanning form
    (let [inst (scan-inst*)]
      (concat (scan-expr) [inst]))))

(defn unfold [{:keys [op form] :as inst}]
  (if op
    [inst]
    (case (first form)
      block (unfold-block form)
      loop (unfold-block form)
      if (unfold-if form)
      (unfold-plain inst))))

;;; Module Fields.

(defmulti -parse-modulefield first)

(defn parse-modulefield [form]
  (check-phrase form)
  (-parse-modulefield form))

(defmethod -parse-modulefield 'type [form]
  (fail "cannot parse/-modulefield 'type"))

(defn scan-importdesc []
  (let [[head & tail :as form] (scan-phrase)
        ast (scanning tail
              (let [id (scanning-opt (scan-id))
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
    (let [id (scanning-opt (scan-id))
          import (scanning-opt (scan-inline-import))]
      (if-let [{:keys [module name]} import]
        (let [type (scan-typeuse)]
          (parse-modulefield (list 'import module name
                                   (concat ['func]
                                           (when id [id])
                                           (:forms type)))))
        (if-let [{:keys [name]} (scanning-opt (scan-inline-export))]
          (let [tail (scan-rest)
                id (fresh-id name)]
            (parse-modulefield (list 'export name (list 'func id)))
            (parse-modulefield (list* 'func id tail)))
          (let [type (scan-typeuse)
                locals (scan-locals)
                body (scan-expr)
                func {:head 'func
                      :form form
                      :id id
                      :type type
                      :locals locals
                      :body body}
                index (emit-field :funcs func)]
            (bind! :funcs index index)))))))

(defmethod -parse-modulefield 'table [form]
  (fail "cannot parse/-modulefield 'table"))

(defn scan-memtype []
  (scan-limits))

(defmethod -parse-modulefield 'memory [[head & tail :as form]]
  (scanning tail
    (let [id (scanning-opt (scan-id))
          ;;TODO: Abbreivation: inline data.
          type (scan-memtype)
          memory {:head 'memory
                  :form form
                  :id id
                  :type type}
          index (emit-field :mems memory)]
      (bind! :mems index index))))

(defmethod -parse-modulefield 'global [form]
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

(defmethod -parse-modulefield 'start [[head & tail :as form]]
  (change! *module* update :fields conj [:start])
  (let [start (scanning tail
                (let [id (scan-id)]
                  {:sort :modulefield
                   :head head
                   :func {:section :funcs :id id}
                   :form form}))]
    (change! *module* assoc :start start)))

(defn scan-offset []
  (if-let [[_ & tail :as form] (scanning-opt (scan-phrase 'offset))]
    (let [expr (scanning tail
                 (scan-expr))]
      {:head 'offset
       :expr expr
       :form form})
    (let [expr (scan-expr)]
      {:head 'offset
       :expr expr})))

(defmethod -parse-modulefield 'elem [[head & tail :as form]]
  (scanning tail
    (let [table {:id (or (scanning-opt (scan-index)) 0)
                 :section :tables}
          offset (scan-offset)
          init (scan-all scan-index)
          element {:head head
                   :table table
                   :offset offset
                   :init init
                   :form form}]
      (emit-field :elements element))))

(defn scan-datastring []
  (scan-pred #(or (string? %) (vector? %))))

(defn scan-bytes []
  (let [w (io/new-array-writer)]
    (doseq [s (scan-all scan-datastring)]
      (if (vector? s)
        (run! #(io/write-byte w %) s)
        (io/write-bytes w (io/utf-8-bytes s))))
    (io/array-writer-bytes w)))

(defmethod -parse-modulefield 'data [[head & tail :as form]]
  (scanning tail
    (let [memory {:id (or (scanning-opt (scan-index)) 0)
                  :section :mems}
          offset (scan-offset)
          init (scan-bytes)
          data {:head head
                :memory memory
                :offset offset
                :init init
                :form form}]
      (emit-field :data data))))

;;; Modules.

(def empty-vecsec {:env {} :fields []})

(defn parse-module* [[head & tail :as form]]
  (binding [*module* {:sort :toplevel
                      :head head
                      :form form
                      :counter 0
                      :signatures {}
                      :fields []
                      :types empty-vecsec
                      :funcs empty-vecsec
                      :imports empty-vecsec
                      :exports empty-vecsec
                      :mems empty-vecsec
                      :data empty-vecsec
                      :start nil}]
      (run! parse-modulefield tail)
      *module*))

(defn parse-module [forms]
  (if (has-head? 'module (first forms))
    (if (next forms)
      (fail "unexpected form after module")
      (parse-module* (first forms)))
    (parse-module* (list* 'module forms))))
