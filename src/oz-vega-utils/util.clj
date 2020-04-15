(ns oz-vega-utils.util)

(defn map-if
  [f pred coll]
  (map #(cond-> % (pred %) f) coll))

(comment
  (map-if inc even? [100 10 3 1001 5]))

(defn map-vals
  [f m]
  (into {} (map (juxt key (comp f val)) m)))

(defn index-of-elem-with-kv
  [coll k v]
  (->> coll
    (map-indexed vector)
    (filter #(= v (-> % second k)))
    first
    first))

(defn update-in-with-fn
  [coll [fn-or-k & fns-and-ks] f & args]
  (let [k        (if (fn? fn-or-k)
                   (fn-or-k coll)
                   fn-or-k)
        [f args] (if (seq fns-and-ks)
                   [update-in-with-fn (into [fns-and-ks f] args)]
                   [f args])]
    (if (nil? k)
      (throw (ex-info "Supplied fn did not find a key" {:kv   (:kv (meta fn-or-k))
                                                        :coll coll}))
      (apply update coll k f args))))

(defn assoc-in-with-fn
  [coll [fn-or-k & fns-and-ks] val]
  (let [k (if (fn? fn-or-k)
            (fn-or-k coll)
            fn-or-k)]
    (cond
      (nil? k)         (throw (ex-info "Supplied fn did not find a key" {:kv   (:kv (meta fn-or-k))
                                                                         :coll coll}))
      (seq fns-and-ks) (update coll k assoc-in-with-fn fns-and-ks val)
      :else            (assoc coll k val))))

(defn get-in-with-fn
  [coll [fn-or-k & fns-and-ks]]
  (let [k (if (fn? fn-or-k)
            (fn-or-k coll)
            fn-or-k)]
    (-> coll
      (get k)
      (cond-> (seq fns-and-ks) (get-in-with-fn fns-and-ks)))))

(defn ks->kv-index
  [ks]
  (map-if (fn [[k v :as kv]] (vary-meta #(index-of-elem-with-kv % k v) merge {:kv kv})) coll? ks))

(defn get-in-with-kv-index
  [coll ks-and-kvs]
  (->> ks-and-kvs
    ks->kv-index
    (get-in-with-fn coll)))

(defn assoc-in-with-kv-index
  [coll ks-and-kvs val]
  (assoc-in-with-fn coll (ks->kv-index ks-and-kvs) val))

(defn update-in-with-kv-index
  [coll ks-and-kvs f & args]
  (let [ks-and-kvs (ks->kv-index ks-and-kvs)]
    (apply update-in-with-fn coll ks-and-kvs f args)))

(comment
  (meta (second (ks->kv-index [:m [:l :n] 0])))
  (get-in-with-kv-index {:marks [{:name      :nodes
                                  :transform [{:type   :force
                                               :forces 5}]}]}
    [:marks  [:name :nodes] :transform [:type :force] :forces])
  (assoc-in-with-kv-index {:marks [{:name      :nodes
                                    :transform [{:type   :force
                                                 :forces 5}]}]}
    [:marks  [:name :nodes] :transform [:type :force] :forces] 9)
  (update-in-with-kv-index {:marks [{:name      :nodes
                                     :transform [{:type   :force
                                                  :forces 5}]}]}
    [:marks  [:name :nodes] :transform [:type :force] :forces]
    assoc :x :y))
