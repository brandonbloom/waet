

(defn unify-type [t]
  (let [{:keys [id]} t
        ts (:signature t)
        index (or (when (index? id)
                    id)
                  (when (id? id)
                    (prn 'id= id 'found= (get-in *module* [:types :env id]))
                    (get-in *module* [:types :env id]))
                  (when ts
                    (get *signatures* ts))
                  (let [index (count *types*)
                        t (assoc t :index index)]
                    (change! *types* conj t)
                    index))
        t (assoc t :index index)
        u (get *types* index)
        us (:signature u)
        x (cond
            (= ts us) ts
            (and ts (nil? us)) ts
            (and (nil? ts) us) us
            :else (fail "types don't match" {:t t :u u}))]
    (when (:signature x)
      (change! *types* assoc-in [(:index t) :signature] (:signature x))
      (change! *types* assoc-in [(:index u) :signature] (:signature x)))
    (merge t (select-keys x [:params :results :signature]))))
