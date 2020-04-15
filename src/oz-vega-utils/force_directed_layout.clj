(ns oz-vega-utils.force-directed-layout
  (:require [oz.core :as oz]
            [clojure.string :as str]))

(defn index-of-elem-with-kv
  [coll k v]
  (->> coll
       (map-indexed vector)
       (filter #(= v (-> % second k)))
       first
       first))

(defn- do-update-in-with-fn
  [coll p ksf f args]
  (let [i         (if (fn? p) (p coll) p)
        this-f    (if (seq ksf) do-update-in-with-fn f)
        this-args (if (seq ksf) [(first ksf) (rest ksf) f args] args)]
    (apply update coll i this-f this-args)))

(defn update-in-with-fn
  [coll ksf f & args]
  (do-update-in-with-fn coll (first ksf) (rest ksf) f args))

(defn update-in-with-kv-index
  [coll ksf f & args]
  (let [ksf (map (fn [p] (if (coll? p)
                          #(index-of-elem-with-kv % (first p) (second p))
                          p))
                 ksf)]
    (apply update-in-with-fn coll ksf f args)))

(comment
  (update-in-with-kv-index {:marks [{:name      :nodes
                                     :transform [{:type   :force
                                                  :forces 5}]}]}
                           [:marks  [:name :nodes] :transform [:type :force] :forces]
                           inc))

(defn force-property-sym
  [force-name property-name]
  (str (name force-name) (str/capitalize (name property-name))))

(defn map-vals
  [f m]
  (into {} (map (juxt key (comp f val)) m)))

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
                           (let [name (or (:name value) (force-property-sym force property))]
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

(defn add-force
  [m force props]
  (let [prop-sel-map (props->prop-sel-map force props)
        m            (add-signals m prop-sel-map)]
    (update-in-with-kv-index m [:marks [:name :nodes] :transform [:type :force] :forces]
                             (fn [forces]
                               (->> prop-sel-map
                                    (map-vals #(cond-> % (coll? %) (->> :name (hash-map :signal))))
                                    (merge {:force force})
                                    (conj forces))))))
(comment
  (-> {:signals []
       :marks   [{:name      :nodes
                  :transform [{:type   :force
                               :forces []}]}]}
      (add-force :collide {:radius {:init 1 :min 5 :max 6}})
      ((juxt :signals #(-> % :marks first :transform first :forces first)))))

(defn force-directed-layout
  [{:keys [nodes links]}
   & {:keys [description canvas node-color link-color text-color labeled? sim]
      :or   {canvas     {:height  500
                         :width   700
                         :padding 0}
             link-color {:width 0.5 :stroke "#ccc"}
             node-color {:key "group" :scheme "category20c" :stroke "white"}
             text-color {:stroke "black"}
             sim        {:static?    true
                         :iterations 300}}}]
  (let [node-radius-sym                "nodeRadius"
        fix-sym                        "fix"
        restart-sym                    "restart"
        static-sym                     "static"
        node-sym                       "node"
        node-data-sym                  "node-data"
        link-data-sym                  "link-data"
        color-sym                      "color"
        center-y-sym                   "centerY"
        x-scale-sym                    "xscale"
        {:keys [width height padding]} canvas]
    {:$schema     "https://vega.github.io/schema/vega/v5.json"
     :description description
     :autosize    "none"
     :width       width
     :height      height
     :padding     padding
     :data        [{:name node-data-sym :values nodes}
                   {:name link-data-sym :values links}]
     :signals     [{:name  static-sym
                    :value (:static? sim)
                    :bind  {:input "checkbox"}}
                   {:description "State variable for active node fix status."
                    :name        fix-sym
                    :value       false
                    :on          [{:events "symbol:mouseout[!event.buttons], window:mouseup"
                                   :update "false"}
                                  {:events "symbol:mouseover" :update (format "%s || true" fix-sym)}
                                  {:events "[symbol:mousedown, window:mouseup] > window:mousemove!"
                                   :update "xy()"
                                   :force  true}]}
                   {:description "Graph node most recently interacted with."
                    :name        node-sym
                    :value       nil
                    :on          [{:events "symbol:mouseover" :update (format "%s === true ? item() : node" fix-sym)}]}
                   {:description "Flag to restart Force simulation upon data changes."
                    :name        restart-sym
                    :value       false
                    :on          [{:events {:signal fix-sym} :update (format "%s && %s.length" fix-sym fix-sym)}]}]
     :scales      [(let [{:keys [key scheme]} node-color]
                     {:name   color-sym
                      :type   :ordinal
                      :domain {:data node-data-sym :field key}
                      :range  {:scheme scheme}})
                   (let [{:keys [key]} node-color]
                     {:name   x-scale-sym
                      :type   :band
                      :domain {:data node-data-sym :field key}
                      :range  "width"})]
     :axes        [{:orient :bottom
                    :scale  x-scale-sym}]
     :marks       (cond-> [{:name      :nodes
                            :type      :symbol
                            :zindex    1
                            :from      {:data node-data-sym}
                            :on        [{:trigger fix-sym
                                         :modify  node-sym
                                         :values  (format "%s === true ? {fx: node.x, fy: node.y} : {fx: %s[0], fy: %s[1]}" fix-sym fix-sym fix-sym)}
                                        {:trigger (format "!%s" fix-sym) :modify node-sym :values "{fx: null, fy: null}"}]
                            :encode    {:enter  (let [{:keys [key stroke]} node-color]
                                                  {:fill   {:scale color-sym :field key}
                                                   :stroke {:value stroke}
                                                   :name   {:field "name"}
                                                   :xfocus {:scale x-scale-sym :field key :band 0.5}
                                                   :yfocus {:signal center-y-sym}})
                                        :update {:size   {:signal (str "2 * " node-radius-sym " * " node-radius-sym)}
                                                 :cursor {:value :pointer}}}
                            :transform [{:type       :force
                                         :iterations (:iterations sim)
                                         :restart    {:signal restart-sym}
                                         :static     {:signal static-sym}
                                         :signal     :force}]}
                           {:type        :path
                            :from        {:data link-data-sym}
                            :interactive false
                            :encode      {:update (let [{:keys [stroke width]} link-color]
                                                    {:stroke      {:value stroke}
                                                     :strokeWidth {:value width}})}
                            :transform   [{:type    :linkpath
                                           :require {:signal :force}
                                           :shape   :line
                                           :sourceX "datum.source.x"
                                           :sourceY "datum.source.y"
                                           :targetX "datum.target.x"
                                           :targetY "datum.target.y"}]}]
                    labeled? (conj {:type   :text
                                    :from   {:data :nodes}
                                    :zindex 2
                                    :encode {:enter  (let [{:keys [stroke]} text-color]
                                                       {:text   {:field "name"}
                                                        :stroke {:value stroke}
                                                        :align  {:value "center"}})
                                             :update {:x {:field "x"}
                                                      :y {:field "y"}}}}) )}))

(def data (oz/load"/home/harrison/Downloads/miserables.json"))

(let [width  1200
      height 500]
  (-> data
      (force-directed-layout
        :description "A node-link diagram with force-directed layout, depicting character co-occurrence in the novel Les Misérables."
        :labeled? true
        :canvas {:width  width
                 :height height}
        :sim {:static? false})
      (add-force :collide
                 {:radius   {:name "nodeRadius"
                             :init 10 :min 1 :max 50}
                  :strength {:init 0.7 :min 0.1 :max 1 :step 0.1}})
      (add-force :nbody
                 {:strength        {:init -30 :min -100 :max 10}
                  :theta           {:init 0.9 :min 0.1 :max 1 :step 0.1}
                  #_#_:distanceMin {:init 1 :min 0 :max 100}
                  #_#_:distanceMax {:init 1 :min 0 :max 100}})
      (add-force :link
                 {:links        "link-data"
                  :distance     {:init 30 :min 5 :max 100}
                  #_#_:strength {:init 0.7 :min 0.1 :max 1 :step 0.1}})
      (add-force :center
                 {:x {:init (/ width 2)}
                  :y {:init (/ height 2)}})
      (add-force :x
                 {:x        "xfocus"
                  :strength {:init 0.1 :min 0.1 :max 1 :step 0.1}})
      (add-force :y
                 {:y        "yfocus"
                  :strength 0.1})
      (oz/view! :mode :vega)))
