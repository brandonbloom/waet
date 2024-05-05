(ns waet.parse
  (:use [waet.util])
  (:require [waet.values :as val]
            [waet.impl.scan :refer [bad-syntax *pos*] :as scan]
            [waet.inst :as inst]
            [waet.io :as io]))

;;;; See <https://webassembly.github.io/spec/core/text/index.html>.

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

(defn fresh-id
  ([] (fresh-id ""))
  ([name]
   (let [n (:counter *module*)]
     (change! *module* update :counter inc)
     (symbol (str "$" name "__" n))))) ;TODO: Something less likely to collide.

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

(defn scan-u8  [] (scan/pred val/u8?))
(defn scan-u16 [] (scan/pred val/u16?))
(defn scan-u32 [] (scan/pred val/u32?))
(defn scan-u64 [] (scan/pred val/u64?))

(defn scan-f32 [] (scan/pred val/f32?))
(defn scan-f64 [] (scan/pred val/f64?))

(defn scan-signed [n signed? unsigned?]
  (let [i (scan/pred #(or (signed? %) (unsigned? %)))]
    ;; Numbers larger than the signed max are coerced as twos-complement.
    ;; See <https://webassembly.github.io/spec/core/syntax/values.html#syntax-int>
    ;; and <https://webassembly.github.io/spec/core/exec/numerics.html#aux-signed>.
    (long
      (if (and (unsigned? i) (not (signed? i)))
        (- i (.shiftLeft (biginteger 1N) n))
        i))))

(defn scan-i8  [] (scan-signed 8 val/i8?  val/u8?))
(defn scan-i16 [] (scan-signed 16 val/i16? val/u16?))
(defn scan-i32 [] (scan-signed 32 val/i32? val/u32?))
(defn scan-i64 [] (scan-signed 64 val/i64? val/u64?))

(defn scan-id []
  (scan/pred val/id?))

(defn scan-index
  ([]
   ;; Either symbolic or numeric index.
   (scan/pred #(or (val/id? %) (val/index? %))))
  ([section]
   (let [id (scan-index)]
     {:id id
      :section section
      :near *pos*})))


(defn scan-name []
  (scan/pred val/name?))

(defn scan-phrase
  ([]
   (scan/pred phrase?))
  ([head]
   (scan/pred #(has-head? head %))))

(defn scan-import []
  (scan-phrase 'import))

(defn scan-typeid []
  (let [[_ & tail :as form] (scan-phrase 'type)]
    (scan/from tail
      (let [id (scan-id)]
        {:head 'type
         :near *pos*
         :id id}))))

(defn scan-valtype []
  (scan/pred '#{i32 i64 f32 f64}))

(defn scan-local* [head]
  (let [[_ & tail :as form] (scan-phrase head)]
    (scan/from tail
      (let [[id types] (if-let [id (scan/optional (scan-id))]
                         (let [type (scan-valtype)]
                           [id [type]])
                         [nil (scan/zom scan-valtype)])]
        (mapv (fn [type]
                {:head head
                 :near *pos*
                 :id id
                 :type type
                 :form form})
              types)))))

(defn scan-locals* [head]
  (loop [v []]
    (if-let [x (scan/optional (scan-local* head))]
      (recur (into v x))
      v)))

(defn scan-locals []
  (scan-locals* 'local))

(defn scan-params []
  (scan-locals* 'param))

(defn scan-results []
  (scan-locals* 'result))

(defn ensure-type [{:keys [params results] :as type}]
  (let [signature [(mapv :type params)
                   (mapv :type results)]
        index (or (get-in *module* [:signatures signature])
                  (let [index (emit-field :types type)]
                    (change! *module* assoc-in [:signatures signature] index)
                    index))]
    (get-in *module* [:types :fields index])))

(defn scan-typeuse []
  (let [typeid (scan/optional (scan-typeid))
        params (scan-params)
        results (scan-results)
        forms (concat (when typeid
                        [(:form typeid)])
                      (map :form params)
                      (map :form results))
        type {:head 'func
              :id (:id typeid)
              :params params
              :results results}]
    (ensure-type type)))

(defn scan-functype []
  (let [[_ & tail :as form] (scan-phrase 'func)
        params (scan-params)
        results (scan-results)]
    {:head 'func
     :params params
     :results results
     :form form}))

(def scan-type scan-functype)

(defn scan-sharing []
  (scan/pred '#{shared unshared}))

(defn scan-limits []
  (let [n (scan-u32)
        m (scan/optional (scan-u32))
        sharing (scan/optional (scan-sharing))]
    {:min n
     :max (or m n)
     :shared? (= sharing 'shared)}))

(def scan-memtype scan-limits)

(defn scan-elemtype []
  (scan/pred #{'anyfunc}))

(defn scan-tabletype []
  (let [limits (scan-limits)
        elemtype (scan-elemtype)]
    {:limits limits
     :elemtype elemtype}))

(defn scan-globaltype []
  (let [var (scan/optional (scan-phrase 'mut))]
    (if-let [[head & tail] var]
      (scan/from tail
        (let [type (scan-valtype)]
          {:head head
           :kind :var
           :type type}))
      (let [type (scan-valtype)]
        {:kind :const
         :type type}))))

(defn scan-inline-export []
  (let [form (scan-phrase 'export)]
    (scan/from (next form)
      (let [name (scan-name)]
        {:head 'export
         :form form
         :name name}))))

(defn scan-inline-import []
  (let [form (scan-phrase 'import)]
    (scan/from (next form)
      (let [module (scan-name)
            name (scan-name)]
        {:head 'import
         :form form
         :module module
         :name name}))))

;;; Instructions.

(defn op? [x]
  (and (symbol? x)
       (not (val/id? x))))

(declare scan-inst unfold)

(defn scan-body []
  (loop [body []]
    (let [{:keys [op] :as inst} (scan-inst)]
      (if ('#{end else} op)
        [body op]
        (recur (into body (unfold inst)))))))

(defn scan-label []
  (scan/optional (scan-index :labels)))

(defn scan-block []
  (let [label (scan-label)
        results (scan-results)
        [body terminator] (scan-body)
        id (scan/optional (scan-id))]
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
  (let [labels (scan/zom scan-label)]
    {:branches (vec (butlast labels))
     :default (last labels)}))

(defn scan-kwarg [key scan-arg default]
  (let [value (if (scan/optional (scan/pred #{(symbol (str (name key) "="))}))
                (scan-arg)
                default)]
    {key value}))

(defn scan-memarg [align]
  (merge (scan-kwarg :offset scan-u32 0)
         (scan-kwarg :align scan-u32 align)))

(defn scan-f32-bits []
  (Float/floatToIntBits (scan-f32)))

(defn scan-f64-bits []
  (Double/doubleToLongBits (scan-f64)))

(defn -scan-v128 [scan-lane bits lanes]
  (reduce (fn [acc n]
            (+ (.shiftLeft (biginteger acc) bits) n))
          0N
          (repeatedly lanes scan-lane)))

(defn scan-v128 []
  (let [format (scan/one)]
    (case format
      i8x16 (-scan-v128 scan-i8 8 16)
      i16x8 (-scan-v128 scan-i16 16 8)
      i32x4 (-scan-v128 scan-i32 32 4)
      i64x2 (-scan-v128 scan-i64 64 2)
      f32x4 (-scan-v128 scan-f32-bits 32 4)
      f64x2 (-scan-v128 scan-f64-bits 64 2)
      (bad-syntax format "unsupported simd format" {:format format}))))

(defn scan-op []
  (let [id (scan/pred op?)]
    (or (inst/by-name id)
        (bad-syntax id "unknown instruction" {:op id}))))

(defn scan-immediate [immediate]
  (case immediate

    :block (scan-block)
    :if (scan-then+else)
    :label {:label (scan-label)}
    :br_table (scan-branches)

    :call {:func (scan-index :funcs)}
    :call_indirect {:type (scan-typeuse)}

    :local  {:local (scan-index :locals)}
    :global {:global (scan-index :globals)}

    :mem1  (scan-memarg 1)
    :mem2  (scan-memarg 2)
    :mem4  (scan-memarg 4)
    :mem8  (scan-memarg 8)
    :mem16 (scan-memarg 16)
    :mem32 (scan-memarg 32)
    :mem64 (scan-memarg 64)

    :tag {:tag (scan-index :tags)}

    :i32 {:value (scan-i32)}
    :i64 {:value (scan-i64)}
    :f32 {:value (scan-f32)}
    :f64 {:value (scan-f64)}
    :v128 {:value (scan-v128)}

    (fail "Unsupported immediate" {:immediate immediate})))

(defn scan-inst* []
  (let [{:keys [immediates] op :name} (scan-op)]
    (reduce (fn [instruction immediate]
              (let [args (scan-immediate immediate)]
                (doseq [k (keys args)]
                  ;; Previously, only a single immediate argument was supported,
                  ;; so parameter names were baked into the immediate type.
                  ;; Now that multiple immediates are supported, continue to
                  ;; assume the names are distinct... until they aren't and
                  ;; this check fails. This delays refactoring downstream
                  ;; consumers of instructions.
                  (when (contains? instruction k)
                    (fail "duplicate arg key" {:key k})))
                (merge instruction args)))
              {:op op}
              immediates)))

(defn scan-inst []
  (if-let [[op & _ :as form] (scan/optional (scan-phrase))]
    {:form form}
    (scan-inst*)))

(defn scan-expr []
  (vec (mapcat unfold (scan/zom scan-inst))))

(defn parse-body [[head & tail :as form]]
  (scan/from tail
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
  (scan/from form
    (scan/pred #{'if})
    (let [label (scan-label)
          results (scan-results)
          then (-> (scan-phrase 'then)
                   parse-body)
          else (-> (scan/optional (scan-phrase 'else))
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
  (scan/from form
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
  (scan/set-pos-from form)
  (-parse-modulefield form))

(defmethod -parse-modulefield 'type [[head & tail :as form]]
  (scan/from tail
    (let [id (scan/optional (scan-id))
          type (scan-type)]
      (ensure-type type))))

(defn scan-importdesc []
  (let [[head & tail :as form] (scan-phrase)
        ast (scan/from tail
              (let [id (scan/optional (scan-id))
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
  (scan/from tail
    (let [module (scan-name)
          name (scan-name)
          desc (scan-importdesc)
          ast {:head head
               :form form
               :module module
               :name name
               :desc desc}]
      (emit-field :imports ast))))

(defn parse-named [[head & tail :as form] scan-and-emit]
  (scan/from tail
    (let [id (scan/optional (scan-id))
          import (scan/optional (scan-inline-import))]
      (if-let [{:keys [module name]} import]
        (let [forms (scan/tail)]
          (parse-modulefield (list 'import module name
                                   (concat ['func]
                                           (when id [id])
                                           forms))))
        (if-let [{:keys [name]} (scan/optional (scan-inline-export))]
          (let [tail (scan/tail)
                id (fresh-id name)]
            (parse-modulefield (list 'export name (list head id)))
            (parse-modulefield (list* head id tail)))
          (scan-and-emit id))))))

(defmethod -parse-modulefield 'func [form]
  (parse-named form
    (fn [id]
      (let [type (scan-typeuse)
            locals (scan-locals)
            body (scan-expr)
            func {:head 'func
                  :form form
                  :id id
                  :type type
                  :locals locals
                  :body body}]
        (emit-field :funcs func)))))

(defmethod -parse-modulefield 'table [[head & tail :as form]]
  (parse-named form
    (fn [id]
      (if-let [elemtype (scan/optional (scan-elemtype))]
        (let [[_ & elems] (scan-phrase 'elem)
              n (count elems)
              id (or id (fresh-id))]
          (parse-modulefield (list 'table id n n elemtype))
          (parse-modulefield (list* 'elem id (list 'i32.const 0) elems)))
        (let [type (scan-tabletype)
              table {:head head
                     :type type
                     :form form}]
          (emit-field :tables table))))))

(defmethod -parse-modulefield 'memory [[head & tail :as form]]
  (parse-named form
    (fn [id]
      (let [;;TODO: Abbreivation: inline data.
           type (scan-memtype)
           memory {:head 'memory
                   :form form
                   :id id
                   :type type}]
        (emit-field :mems memory)))))

(defmethod -parse-modulefield 'tag [[head & tail :as form]]
  (parse-named form
    (fn [id]
      (let [tag {:id id}]
        (emit-field :tags tag)))))

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
    (scan/from tail
      (let [index (scan-index)]
        {:sort :exportdesc
         :head head
         :form form
         :section section
         :id index})))) ; TODO [index->id].

(defmethod -parse-modulefield 'export [[head & tail :as form]]
  (scan/from tail
    (let [name (scan-name)
          desc (scan-exportdesc)
          ast {:head head
               :form form
               :name name
               :desc desc}]
      (emit-field :exports ast))))

(defmethod -parse-modulefield 'start [[head & tail :as form]]
  (change! *module* update :fields conj [:start])
  (let [start (scan/from tail
                (let [id (scan-id)]
                  {:sort :modulefield
                   :head head
                   :func {:section :funcs :id id}
                   :form form}))]
    (change! *module* assoc :start start)))

(defn scan-offset []
  (if-let [[_ & tail :as form] (scan/optional (scan-phrase 'offset))]
    (let [expr (scan/from tail
                 (scan-expr))]
      {:head 'offset
       :expr expr
       :form form})
    (let [expr (scan-expr)]
      {:head 'offset
       :expr expr})))

(defmethod -parse-modulefield 'elem [[head & tail :as form]]
  (scan/from tail
    (let [id (or (scan/optional (scan-index)) 0)
          table {:id id
                 :near *pos*
                 :section :tables}
          offset (scan-offset)
          init (scan/zom scan-index)
          element {:head head
                   :table table
                   :offset offset
                   :init init
                   :form form}]
      (emit-field :elements element))))

(defn scan-datastring []
  (scan/pred #(or (string? %) (val/data? %))))

(defn scan-data []
  (apply val/make-data (scan/zom scan-datastring)))

(defn scan-bytes []
  (let [w (io/new-array-writer)
        data (scan-data)]
    (doseq [chunk (val/data-chunks data)]
      (cond
        (string? chunk) (io/write-bytes w (io/utf-8-bytes chunk))
        (number? chunk) (io/write-byte w chunk)
        :else (fail "unsupported chunk type" {:type (type chunk)})
        ))
    (io/bytes-copy w)))

(defmethod -parse-modulefield 'data [[head & tail :as form]]
  (scan/from tail
    (let [id (or (scan/optional (scan-index)) 0)
          memory {:id id
                  :near *pos*
                  :section :mems}
          offset (scan/optional (scan-offset))
          init (scan-bytes)
          data {:head head
                :memory memory
                :offset offset
                :mode (if offset :active :passive)
                :init init
                :form form}]
      (emit-field :data data))))

;;; Modules.

(def empty-vecsec {:env {} :fields []})

(defn parse-module* [[head & tail :as form]]
  (binding [*pos* nil
            *module* {:sort :toplevel
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
