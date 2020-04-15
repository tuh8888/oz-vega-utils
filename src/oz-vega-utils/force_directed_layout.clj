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

(defn update-in-with-fn
  [coll [fn-or-k & fns-and-ks] f & args]
  (let [k        (if (fn? fn-or-k)
                   (fn-or-k coll)
                   fn-or-k)
        [f args] (if (seq fns-and-ks)
                   [update-in-with-fn (into [fns-and-ks f] args)]
                   [f args])]
    (apply update coll k f args)))

(defn get-in-with-fn
  [coll [fn-or-k & fns-and-ks]]
  (let [k (if (fn? fn-or-k)
            (fn-or-k coll)
            fn-or-k)]
    (-> coll
        (get k)
        (cond-> (seq fns-and-ks) (get-in-with-fn fns-and-ks)))))

(defn map-if
  [f pred coll]
  (map #(cond-> % (pred %) f) coll))

(comment
  (map-if inc even? [100 10 3 1001 5]))

(defn ks->kv-index
  [ks]
  (map-if (fn [[k v]] #(index-of-elem-with-kv % k v)) coll? ks))

(defn get-in-with-kv-index
  [coll ks-and-kvs]
  (->> ks-and-kvs
       ks->kv-index
       (get-in-with-fn coll)))

(defn update-in-with-kv-index
  [coll ks-and-kvs f & args]
  (let [ks-and-kvs (ks->kv-index ks-and-kvs)]
    (apply update-in-with-fn coll ks-and-kvs f args)))

(comment
  (get-in-with-kv-index {:marks [{:name      :nodes
                                  :transform [{:type   :force
                                               :forces 5}]}]}
                        [:marks  [:name :nodes] :transform [:type :force] :forces])
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

(defn add-colors
  [vega sym mark {:keys [data field type scheme] :or {scheme "category20c"}}]
  (-> vega
      (update :scales conj {:name   sym
                            :type   type
                            :domain {:data data :field field}
                            :range  {:scheme scheme}})
      (update-in-with-kv-index [:marks [:name mark] :encode :enter :fill] assoc :scale sym :field field)))

(comment
  (let [canvas {:height  500
                :width   1200
                :padding 10}]
    (-> canvas
        vega-template
        (assoc :marks [{:name   :nodes
                        :encode {:enter {:fill {}}}}])
        (add-colors "color" :nodes {:data "node-data" :field "group" :type :ordinal :scheme "xyc"})
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

(comment
  (-> {}
      vega-template
      (add-axis {:type :ordinal :range scheme :data node-data-sym :field "group"})
      (add-axis {:orient :bottom :type :band :data node-data-sym :field "group" :range "width"})))

(defn add-group-gravity
  [vega sym mark {:keys [field data strength axis]}]
  (let [[range orient] (if (= :x axis)
                         ["width" :bottom]
                         ["height" :left])
        focus-sym      (str sym "Focus")]
    (-> vega
        (add-axis sym {:orient orient :data data :type :band :range range :field field})
        (update-in-with-kv-index [:marks [:name mark] :encode :enter] assoc focus-sym {:scale sym :field field :band 0.5})
        (add-force axis {axis      focus-sym
                         :strength strength}))))

(comment
  (-> canvas
      vega-template
      (add-group-gravity "xscale" :nodes "group" "node-data" {:init 0.1 :min 0.1 :max 1 :step 0.1})))

(defn add-nodes
  [vega nodes-sym data-sym nodes node-radius-sym node-color]
  (-> vega
      (update :data conj {:name data-sym :values nodes})
      (update :marks conj
              {:name      nodes-sym
               :type      :symbol
               :zindex    1
               :from      {:data data-sym}
               :on        []
               :encode    {:enter  (let [{:keys [stroke]} node-color]
                                     {:stroke {:value stroke}
                                      :name   {:field "name"}})
                           :update {:size {:signal (str "2 * " node-radius-sym " * " node-radius-sym)}}}
               :transform []})))

(defn add-force-sim
  [vega fix-sym restart-sym static-sym nodes-sym selected-node-sym {:keys [iterations static?] :as sim}]
  (-> vega
      (update :signals conj {:name  static-sym
                             :value (:static? sim)
                             :bind  {:input "checkbox"}})
      (update :signals conj {:name  fix-sym
                             :value false
                             :on    [{:events "symbol:mouseout[!event.buttons], window:mouseup"
                                      :update "false"}
                                     {:events "symbol:mouseover"
                                      :update (format "%s || true" fix-sym)}
                                     {:events "[symbol:mousedown, window:mouseup] > window:mousemove!"
                                      :update "xy()"
                                      :force  true}]})
      (update :signals conj {:name  selected-node-sym
                             :value nil
                             :on    [{:events "symbol:mouseover"
                                      :update (format "%s === true ? item() : node" fix-sym)}]})
      (update :signals conj {:name  restart-sym
                             :value false
                             :on    [{:events {:signal fix-sym}
                                      :update (format "%s && %s.length" fix-sym fix-sym)}]})
      (update-in-with-kv-index [:marks [:name nodes-sym] :transform] conj {:type       :force
                                                                           :iterations (:iterations sim)
                                                                           :restart    {:signal restart-sym}
                                                                           :static     {:signal static-sym}
                                                                           :signal     :force})
      (update-in-with-kv-index [:marks [:name nodes-sym] :on] conj {:trigger fix-sym
                                                                    :modify  selected-node-sym
                                                                    :values  (format "%s === true ? {fx: node.x, fy: node.y} : {fx: %s[0], fy: %s[1]}"
                                                                                     fix-sym fix-sym
                                                                                     fix-sym)})
      (update-in-with-kv-index [:marks [:name nodes-sym] :on] conj {:trigger (format "!%s" fix-sym)
                                                                    :modify  selected-node-sym
                                                                    :values  "{fx: null, fy: null}"})
      (update-in-with-kv-index [:marks [:name nodes-sym] :encode :update] assoc :cursor {:value :pointer})))

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
  (let [node-radius-sym "nodeRadius"
        fix-sym         "fix"
        restart-sym     "restart"
        static-sym      "static"
        node-sym        "node"
        node-data-sym   "node-data"
        link-data-sym   "link-data"]
    (-> canvas
        vega-template
        (update :data conj {:name node-data-sym :values nodes})
        (update :data conj {:name link-data-sym :values links})
        (update :marks conj {:name      :nodes
                             :type      :symbol
                             :zindex    1
                             :from      {:data node-data-sym}
                             :on        []
                             :encode    {:enter  (let [{:keys [stroke]} node-color]
                                                   {:stroke {:value stroke}
                                                    :name   {:field "name"}})
                                         :update {:size       {:signal (str "2 * " node-radius-sym " * " node-radius-sym)}}}
                             :transform []})

        #_(add-nodes :nodes node-data-sym nodes node-radius-sym node-color)
        (add-force-sim fix-sym restart-sym static-sym :nodes node-sym sim)
        (update :marks conj {:type        :path
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
                                            :targetY "datum.target.y"}]})
        (cond-> labeled? (update :marks conj {:type   :text
                                              :from   {:data :nodes}
                                              :zindex 2
                                              :encode {:enter  (let [{:keys [stroke]} text-color]
                                                                 {:text   {:field "name"}
                                                                  :stroke {:value stroke}
                                                                  :align  {:value "center"}})
                                                       :update {:x {:field "x"}
                                                                :y {:field "y"}}}})))))

(comment
  ;; Initial Setup
  (do
    (oz/live-reload! "src/oz-vega-utils/force_directed_layout.clj")
    (def data (oz/load"/home/harrison/Downloads/miserables.json"))))

(let [width  1200
      height 500]
  (-> data
      (force-directed-layout
        :description "A node-link diagram with force-directed layout, depicting character co-occurrence in the novel Les Misérables."
        :labeled? true
        :canvas {:width  width
                 :height height}
        :sim {:static? false})

      (add-colors "node-color" :nodes {:type   :ordinal
                                       :data   "node-data"
                                       :field  "group"})
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
      (add-group-gravity "x-scale" :nodes {:axis     :x
                                           :field    "group"
                                           :data     "node-data"
                                           :strength {:init 0.1 :min 0.1 :max 1 :step 0.1}})
      (add-group-gravity "y-scale" :nodes {:axis     :y
                                           :field    "group"
                                           :data     "node-data"
                                           :strength {:init 0.5 :min 0.1 :max 2 :step 0.2}})
      #_(add-force :x
                   {:x        "xfocus"
                    :strength {:init 0.1 :min 0.1 :max 1 :step 0.1}})
      #_(add-force :y
                   {:y        "yfocus"
                    :strength 0.1})
      (oz/view! :mode :vega)))
