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
   :syms        #{}
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

(defn validate-syms
  [vega new-syms required-syms]
  (let [available-syms        (-> vega :syms set)
        existing-new-syms     (filter available-syms new-syms)
        missing-required-syms (remove available-syms required-syms)]
    (if (or (seq existing-new-syms)
          (seq missing-required-syms))
      (throw (ex-info "JS symbol requirements not met" {:available        available-syms
                                                        :needed           missing-required-syms
                                                        :already-existing existing-new-syms}))
      (update vega :syms into new-syms))))

(defn prop-sym
  "Generate a JS compatible sym for a prop that is unique within the context of the other provided syms.
  Provide a vector of props to bypass their validation."
  [vega prop & syms]
  (assert (map? vega) "First argument must be a map.")
  (validate-syms vega [] syms)
  (->> prop
    ((if (coll? prop) into conj) (vec syms))
    (map name)
    (interpose "_")
    (apply str)
    keyword))

(defn add-colors
  "Add colors to the specified mark.

  Provides: sym"
  [vega mark {:keys [data field type scheme stroke strokeWidth] :or {scheme "category20c"}}]
  (let [sym (prop-sym vega :colors mark)]
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
  "Add a scale-axis using the data and orientation provided.

  Provides: sym"
  [vega sym {:keys [orient data field type range]}]
  (let [data-sym (prop-sym vega :data data)]
    (-> vega
      (validate-syms [sym] [data-sym])
      (update :scales conj {:name   sym
                            :type   type
                            :domain {:data data-sym :field field}
                            :range  range})
      (update :axes conj {:orient orient
                          :scale  sym}))))

(defn add-signal
  "Add a signal.

  Provides: sym"
  [vega sym required-syms & [m]]
  (-> vega
    (validate-syms  [sym] required-syms)
    (update :signals conj (into {:name sym} m))))

(defn add-checkbox
  "Add a checkbox input signal.

  Provides: sym"
  [vega sym {:keys [init]}]
  (add-signal vega sym [] {:value init
                           :bind  {:input "checkbox"}}))

(defn add-range
  "Add a range input signal.

  Provides: sym."
  [vega sym {:keys [init min max step] :or {step 1 min 0 init 0}}]
  (add-signal vega sym [] {:value init
                           :bind  {:input "range"
                                   :min   min
                                   :max   max
                                   :step  step}}))
