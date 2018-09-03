(ns wasm-clj.visit
  (:use [wasm-clj.util]))

(def ^:dynamic *handler*)

(defmulti visit-children (juxt :sort :head))

(defn begin [ast]
  (*handler* :begin ast))

(defn end [ast]
  (*handler* :end ast))

(defn visit
  ([ast]
   (-> (begin ast) visit-children end))
  ([ast handler]
   (binding [*handler* handler]
     (visit ast))))

(defn visit-section [module key]
  (update-in module [key :fields] #(mapv visit %)))

(defmethod visit-children :default [ast]
  (prn 'default-visit-children ((juxt :sort :head) ast)) ;XXX
  ast)

(defmethod visit-children [:toplevel 'module] [module]
  (-> (begin module)
      (visit-section :funcs)
      (visit-section :exports)
      (visit-section :globals)
      ;XXX elem_segments
      ;XXX data_segments
      (visit-section :starts)
      end))

(defmethod visit-children [:modulefield 'export] [export]
  (update export :desc visit))
