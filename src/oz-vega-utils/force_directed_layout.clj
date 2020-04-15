(ns oz-vega-utils.force-directed-layout
  (:require [oz.core :as oz]
            [oz-vega-utils.core :as ovu]
            [oz-vega-utils.util :as util]))

(defn add-force
  [m force props]
  (let [prop-sel-map (ovu/props->prop-sel-map force props)
        m            (ovu/add-signals m prop-sel-map)]
    (util/update-in-with-kv-index m [:marks [:name :nodes] :transform [:type :force] :forces]
      (fn [forces]
        (->> prop-sel-map
          (util/map-vals #(cond-> % (coll? %) (->> :name (hash-map :signal))))
          (merge {:force force})
          (conj forces))))))
(comment
  (-> {:signals []
       :marks   [{:name      :nodes
                  :transform [{:type   :force
                               :forces []}]}]}
      (add-force :collide {:radius {:init 1 :min 5 :max 6}})
      ((juxt :signals #(-> % :marks first :transform first :forces first)))))

(defn add-group-gravity
  [vega sym mark {:keys [field data strength axis]}]
  (let [[range orient] (if (= :x axis)
                         ["width" :bottom]
                         ["height" :left])
        focus-sym      (str sym "Focus")]
    (-> vega
      (ovu/add-axis sym {:orient orient :data data :type :band :range range :field field})
      (util/assoc-in-with-kv-index [:marks [:name mark] :encode :enter focus-sym] {:scale sym :field field :band 0.5})
      (add-force axis {axis      focus-sym
                       :strength strength}))))

(defn add-force-sim
  [vega fix-sym restart-sym nodes-sym links-sym {:keys [iterations static]
                                                 :or   {iterations 300
                                                        static     {:init true
                                                                    :sym  "static"}}}]
  (-> vega
    (cond-> (:sym static) (update :signals conj {:name  (:sym static)
                                                 :value (:init static)
                                                 :bind  {:input "checkbox"}}))
    (update :signals conj {:name  fix-sym
                           :value false
                           :on    []})
    (update :signals conj {:name  restart-sym
                           :value false
                           :on    [{:events {:signal fix-sym}
                                    :update (ovu/js "%s && %s.length" fix-sym fix-sym)}]})
    (util/update-in-with-kv-index [:marks [:name nodes-sym] :transform] conj {:type       :force
                                                                              :iterations iterations
                                                                              :restart    {:signal restart-sym}
                                                                              :static     (cond-> static
                                                                                            (:sym static) (->> :sym (hash-map :signal)))
                                                                              :signal     :force})
    (util/update-in-with-kv-index [:marks [:name links-sym] :transform] conj {:type    :linkpath
                                                                              :require {:signal :force}
                                                                              :shape   :line
                                                                              :sourceX "datum.source.x"
                                                                              :sourceY "datum.source.y"
                                                                              :targetX "datum.target.x"
                                                                              :targetY "datum.target.y"})))

(defn add-node-dragging
  [vega fix-sym nodes-sym]
  (let [selected-node-sym (ovu/prop-sym nodes-sym :selected)]
    (-> vega
      (update :signals conj {:name  selected-node-sym
                             :value nil
                             :on    [{:events (ovu/js "symbol:mouseover")
                                      :update (ovu/js "%s === true ? item() : %s" fix-sym selected-node-sym)}]})

      (util/update-in-with-kv-index [:signals [:name fix-sym] :on] conj {:events (ovu/js "symbol:mouseout[!event.buttons], window:mouseup")
                                                                         :update "false"})
      (util/update-in-with-kv-index [:signals [:name fix-sym] :on] conj {:events (ovu/js "symbol:mouseover")
                                                                         :update (ovu/js "%s || true" fix-sym)})
      (util/update-in-with-kv-index [:signals [:name fix-sym] :on] conj {:events (ovu/js "[symbol:mousedown, window:mouseup] > window:mousemove!")
                                                                         :update (ovu/js "xy()")
                                                                         :force  true})
      (util/update-in-with-kv-index [:marks [:name nodes-sym] :on] conj {:trigger fix-sym
                                                                         :modify  selected-node-sym
                                                                         :values  (ovu/js "%s === true ? {fx: %s.x, fy: %s.y} : {fx: %s[0], fy: %s[1]}"
                                                                                    fix-sym
                                                                                    selected-node-sym
                                                                                    selected-node-sym
                                                                                    fix-sym
                                                                                    fix-sym)})
      (util/update-in-with-kv-index [:marks [:name nodes-sym] :on] conj {:trigger (ovu/js "!%s" fix-sym)
                                                                         :modify  selected-node-sym
                                                                         :values  (ovu/js "{fx: null, fy: null}")})
      (util/assoc-in-with-kv-index [:marks [:name nodes-sym] :encode :update :cursor :value] :pointer))))

(defn add-nodes
  [vega sym nodes]
  (let  [r (ovu/prop-sym sym :radius)]
    (-> vega
      (update :data conj {:name (ovu/data-sym sym) :values nodes})
      (update :marks conj {:name      sym
                           :type      :symbol
                           :zindex    1
                           :from      {:data (ovu/data-sym sym)}
                           :on        []
                           :encode    {:enter  {}
                                       :update {:size {:signal (ovu/js "2 * %s * %s" r r)}}}
                           :transform []}))))

(defn add-links
  [vega sym links]
  (-> vega
    (update :data conj {:name (ovu/data-sym sym) :values links})
    (update :marks conj {:name        sym
                         :type        :path
                         :from        {:data (ovu/data-sym sym)}
                         :interactive false
                         :encode      {}
                         :transform   []})))

(defn add-node-labels
  [vega nodes-sym label-prop]
  (let [sym (ovu/prop-sym nodes-sym :labels)]
    (-> vega
      (util/assoc-in-with-kv-index [:marks [:name nodes-sym] :encode :enter :label :field] label-prop)
      (update :marks conj {:name   sym
                           :type   :text
                           :from   {:data nodes-sym}
                           :zindex 2
                           :encode {:enter  {:text  {:field :label}
                                             :align {:value :center}}
                                    :update {:x {:field :x}
                                             :y {:field :y}}}}))))

(comment
  ;; Initial Setup
  (do
    (oz/live-reload! "src/oz-vega-utils/force_directed_layout.clj")
    (def data (oz/load"/home/harrison/Downloads/miserables.json"))))


(let [width  1200
      height 500]
  (-> {:width       width
       :height      height
       :description "A node-link diagram with force-directed layout, depicting character co-occurrence in the novel Les Misérables."}
    ovu/vega-template
    (add-nodes :nodes (:nodes data))
    (add-links :links (:links data))
    (add-force-sim :fix :restart :nodes :links {:iterations 300
                                                :static     {:init false
                                                             :sym  :static}})
    (add-node-dragging :fix :nodes)
    (add-node-labels :nodes :name)
    (ovu/add-colors :nodes {:type   :ordinal
                            :data   :nodes_data
                            :field  :group
                            :stroke "white"})
    (ovu/add-colors :links {:type        :static
                            :strokeWidth 0.5
                            :stroke      "#ccc"})
    (ovu/add-colors :nodes_labels {:type   :static
                                   :stroke "black"})
    (add-force :collide
      {:radius   {:name :nodes_radius
                  :init 10 :min 1 :max 50}
       :strength {:init 0.7 :min 0.1 :max 1 :step 0.1}})
    (add-force :nbody
      {:strength        {:init -30 :min -100 :max 10}
       :theta           {:init 0.9 :min 0.1 :max 1 :step 0.1}
       #_#_:distanceMin {:init 1 :min 0 :max 100}
       #_#_:distanceMax {:init 1 :min 0 :max 100}})
    (add-force :link
      {:links        :links_data
       :distance     {:init 30 :min 5 :max 100}
       #_#_:strength {:init 0.7 :min 0.1 :max 1 :step 0.1}})
    (add-force :center
      {:x {:init (/ width 2)}
       :y {:init (/ height 2)}})
    (add-group-gravity :x_scale :nodes {:axis     :x
                                        :field    :group
                                        :data     :nodes_data
                                        :strength {:init 0.1 :min 0.1 :max 1 :step 0.1}})
    (add-group-gravity :y_scale :nodes {:axis     :y
                                        :field    :group
                                        :data     :nodes_data
                                        :strength {:init 0.5 :min 0.1 :max 2 :step 0.2}})
    (oz/view! :mode :vega)))
