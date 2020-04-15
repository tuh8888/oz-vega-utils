(ns oz-vega-utils.core
  (:require [oz-vega-utils.util :as util]))

(defn js
  [s & syms]
  (if (seq syms)
    (->> syms
      (util/map-if name keyword?)
      (apply format s))
    s))

(defn vega-template
  [{:keys [description height width padding]
    :or   {height  500
           width   700
           padding 0}}]
  {:$schema     "https://vega.github.io/schema/vega/v5.json"
   :description description
   :autosize    "none"
   :width       width
   :height      height
   :padding     padding
   :data        []
   :signals     []
   :scales      []
   :axes        []
   :marks       []})

(comment
  (let [canvas {:height  500
                :width   1200
                :padding 10}]
    (-> canvas
      vega-template
      #_(oz/view! :mode :vega))))

(defn prop-sym
  [sym prop & props]
  (->> props
    (into [sym prop])
    (map name)
    (interpose "_")
    (apply str)
    keyword))

(defn add-colors
  [vega mark {:keys [data field type scheme stroke strokeWidth] :or {scheme "category20c"}}]
  (let [sym (prop-sym mark :colors)]
    (if (= :static type)
      (-> vega
        (util/assoc-in-with-kv-index [:marks [:name mark] :encode :update :stroke :value] stroke)
        (util/assoc-in-with-kv-index [:marks [:name mark] :encode :update :strokeWidth :value] strokeWidth))
      (-> vega
        (update :scales conj {:name   sym
                              :type   type
                              :domain {:data data :field field}
                              :range  {:scheme scheme}})
        (util/update-in-with-kv-index [:marks [:name mark] :encode :update :fill] assoc :scale sym :field field)
        (util/assoc-in-with-kv-index [:marks [:name mark] :encode :update :stroke :value] stroke)))))

(comment
  (let [canvas {:height  500
                :width   1200
                :padding 10}]
    (-> canvas
      vega-template
      (assoc :marks [{:name   :nodes
                      :encode {:enter {:fill {}}}}])
      (add-colors :nodes {:data :node-data :field "group" :type :ordinal :scheme "xyc"})
      #_(oz/view! :mode :vega))))

(defn add-axis
  [vega sym {:keys [orient data field type range]}]
  (-> vega
      (update :scales conj {:name   sym
                            :type   type
                            :domain {:data data :field field}
                            :range  range})
      (update :axes conj {:orient orient
                          :scale  sym})))

(defn data-sym
  [sym]
  (prop-sym sym :data))

(defn range-selector
  [name {:keys [init min max step] :or {step 1}}]
  {:name  name
   :value init
   :bind  {:input "range" :min min :max max :step step}})

(defn props->prop-sel-map
  [force props]
  (->> props
    (map  (fn [[property value]]
            [property (if (coll? value)
                        (let [name (or (:name value) (prop-sym force property))]
                          (range-selector name value))
                        value)]))
    (into {})))

(defn add-signals
  [m prop-sel-map]
  (->> prop-sel-map
    (map second)
    (reduce (fn [m sel]
              (cond-> m (coll? sel) (update :signals conj sel)))
      m)))
