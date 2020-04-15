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

(defn prop-sym
  [sym prop]
  (-> sym
    name
    (str "_" (name prop))
    keyword))

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
