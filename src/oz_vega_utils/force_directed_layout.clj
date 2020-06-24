(ns oz-vega-utils.force-directed-layout
  (:require [oz.core :as oz]
            [oz-vega-utils.core :as ovu]
            [oz-vega-utils.util :as util]))

(defn add-force
  "Add a force to the layout of data displayed in mark.
  mark is the name of a mark
  force is the type the force
  props are additional properties for the force. If a map is provided, these will be passed to add-range.

  Provides: force-sym"
  [vega mark force props]
  (let [path      [:marks [:name mark] :transform [:type :force] :forces]
        force-sym (ovu/prop-sym vega force mark)]
    (as-> vega vega
      (ovu/validate-syms vega [force-sym] [mark])
      (util/update-in-with-kv-index vega path conj {:name  force-sym
                                                    :force force})
      (reduce (fn [vega [prop value]]
                ;; If the prop's value is a coll, try to add a range for prop.
                ;; If it exists, check if a signal was provided.
                (let [sym  (or (:signal value) (ovu/prop-sym vega prop force-sym))
                      path (into path [[:force force] prop])]
                  (if (coll? value)
                    (-> (try (ovu/add-range vega sym value)
                             (catch Exception e
                               (if (:signal value) vega (throw e))))
                      (util/assoc-in-with-kv-index (conj path :signal) sym))
                    (util/assoc-in-with-kv-index vega path value))))
        vega props))))

(comment
  (-> {:signals []
       :marks   [{:name      :nodes
                  :transform [{:type   :force
                               :forces []}]}]}
    (add-force :nodes :collide {:radius {:init 1 :min 5 :max 6}})
    ((juxt :signals #(-> % :marks first :transform first :forces first)))))

(defn add-group-gravity
  "Add gravity according to the field for the mark.

  Provides: focus-sym and axis-sym"
  [vega mark {:keys [field type strength axis]
              :or   {type :band}}]
  (let [sym            (ovu/prop-sym vega [:gravity field axis] mark)
        [range orient] (if (= :x axis)
                         ["width" :bottom]
                         ["height" :left])
        focus-sym      (ovu/prop-sym vega [:focus field axis] mark)]
    (-> vega
      (ovu/validate-syms [focus-sym] [mark])
      (ovu/add-axis sym {:orient orient :data mark :type type :range range :field field})
      (util/assoc-in-with-kv-index [:marks [:name mark] :encode :enter focus-sym] {:scale sym :field field :band 0.5})
      (add-force mark axis {axis      focus-sym
                            :strength strength}))))

(defn add-force-sim
  "Add a force simulation for the nodes and lines.
  Provides restart and static syms.
  provide an init value for static to create a toggle for static vs dynamic simulation.

  Provides: restart-sym and static-sym"
  [vega nodes-mark links-mark {:keys [iterations static shape]
                               :or   {iterations 300
                                      shape      :line
                                      static     {:init true}}}]
  (let [restart-sym (ovu/prop-sym vega :restart nodes-mark links-mark)
        static-sym  (ovu/prop-sym vega :static nodes-mark links-mark)]
    (-> vega
      (ovu/validate-syms [restart-sym] [nodes-mark links-mark])
      (cond-> (boolean? (:init static)) (ovu/add-checkbox static-sym static))
      (update :signals conj {:name  restart-sym
                             :value false
                             :on    []})
      #_(ovu/validate-syms [] [static-sym]) ;; I could either re-validate that static-sym was added or leave the if boolean? logic below. Same for radius in add-nodes fn
      (util/update-in-with-kv-index [:marks [:name nodes-mark] :transform] conj {:type       :force
                                                                                 :iterations iterations
                                                                                 :restart    {:signal restart-sym}
                                                                                 :static     (if (boolean? (:init static))
                                                                                               {:signal static-sym}
                                                                                               static)
                                                                                 :signal     :force
                                                                                 :forces     []})
      (util/update-in-with-kv-index [:marks [:name links-mark] :transform] conj {:type    :linkpath
                                                                                 :require {:signal :force}
                                                                                 :shape   shape
                                                                                 :sourceX "datum.source.x"
                                                                                 :sourceY "datum.source.y"
                                                                                 :targetX "datum.target.x"
                                                                                 :targetY "datum.target.y"}))))

(defn add-node-dragging
  "Allow node dragging for nodes-mark.

  Provides: fix-sym, unfix-sym, and selected-node-sym"
  [vega nodes-mark links-mark & [fix-until-fn]]
  (let [fix-sym           (ovu/prop-sym vega :fix nodes-mark links-mark)
        restart-sym       (ovu/prop-sym vega :restart nodes-mark links-mark)
        selected-node-sym (ovu/prop-sym vega :selected nodes-mark links-mark)
        unfix-sym         (ovu/prop-sym vega :unfix nodes-mark links-mark)]
    (-> vega
      (ovu/validate-syms [] [nodes-mark restart-sym links-mark])
      (ovu/add-signal fix-sym [] {:value false})
      (ovu/add-signal unfix-sym [])
      (util/update-in-with-kv-index [:signals [:name restart-sym] :on] conj {:events {:signal fix-sym}
                                                                             :update (ovu/js "%s && %s.length" fix-sym fix-sym)})
      (util/update-in-with-kv-index [:signals [:name restart-sym] :on] conj {:events {:signal unfix-sym}
                                                                             :update (ovu/js "!%s.length && %s.length" fix-sym unfix-sym)})
      (ovu/add-signal selected-node-sym [] {:value nil
                                            :on    [{:events (ovu/js "symbol:mousedown")
                                                     :update (ovu/js "%s === true ? item() : %s" fix-sym selected-node-sym)}]})
      (util/update-in-with-kv-index [:signals [:name unfix-sym] :on] conj {:events (ovu/js "symbol:mousedown")
                                                                           :update (ovu/js "true")})
      (util/update-in-with-kv-index [:signals [:name unfix-sym] :on] conj {:events (ovu/js "symbol:mousemove")
                                                                           :update (ovu/js "false")})
      (util/update-in-with-kv-index [:signals [:name unfix-sym] :on] conj {:events (ovu/js "symbol:mouseup")
                                                                           :update (ovu/js "%s ? datum : null" unfix-sym)})
      (util/update-in-with-kv-index [:signals [:name fix-sym] :on] conj {:events (ovu/js "symbol:mouseout[!event.buttons], window:mouseup")
                                                                         :update (ovu/js "false")})
      (util/update-in-with-kv-index [:signals [:name fix-sym] :on] conj {:events (ovu/js "symbol:mousedown")
                                                                         :update (ovu/js "%s || true" fix-sym)})
      (util/update-in-with-kv-index [:signals [:name fix-sym] :on] conj {:events (ovu/js "[symbol:mousedown, window:mouseup] > window:mousemove!")
                                                                         :update (ovu/js "xy()")
                                                                         :force  true})
      (util/update-in-with-kv-index [:marks [:name nodes-mark] :on] conj {:trigger fix-sym
                                                                          :modify  selected-node-sym
                                                                          :values  (ovu/js "%s === true ? {fx: %s.x, fy: %s.y} : {fx: %s[0], fy: %s[1]}"
                                                                                     fix-sym
                                                                                     selected-node-sym
                                                                                     selected-node-sym
                                                                                     fix-sym
                                                                                     fix-sym)})
      (util/update-in-with-kv-index [:marks [:name nodes-mark] :on] conj {:trigger (ovu/js "!%s && %s" fix-sym unfix-sym)
                                                                          :modify  selected-node-sym
                                                                          :values  (ovu/js "{fx: null, fy: null}")})
      (util/assoc-in-with-kv-index [:marks [:name nodes-mark] :encode :update :cursor :value] :pointer))))

(defn add-nodes
  "Add provided nodes to visualization.
  If an init value for radius is provided, the radius can be changed. Otherwise it will be static.

  Provides: sym, data-sym, and radius-sym"
  [vega sym nodes & {:keys [radius]}]
  (let  [vega       (ovu/validate-syms vega [sym] [])
         radius-sym (ovu/prop-sym vega :radius sym)
         data-sym   (ovu/prop-sym vega :data sym)]
    (-> vega
      (ovu/validate-syms [data-sym] [])
      (cond-> (:init radius) (ovu/add-range radius-sym radius))
      (update :data conj {:name data-sym :values nodes})
      (update :marks conj {:name      sym
                           :type      :symbol
                           :zindex    1
                           :from      {:data data-sym}
                           :on        []
                           :encode    {:enter  {:size radius}
                                       :update {:size (if (:init radius)
                                                        {:signal (ovu/js "4 * %s * %s" radius-sym radius-sym)}
                                                        radius)}}
                           :transform []}))))

(defn add-links
  "Add provided nodes to visualization.

  Provides: sym and data-sym"
  [vega sym links]
  (let [vega     (ovu/validate-syms vega [sym] [])
        data-sym (ovu/prop-sym vega :data sym)]
    (-> vega
      (ovu/validate-syms [data-sym] [])
      (update :syms into [sym data-sym])
      (update :data conj {:name data-sym :values links})
      (update :marks conj {:name        sym
                           :type        :path
                           :from        {:data data-sym}
                           :interactive false
                           :encode      {:enter  {}
                                         :update {}}}))))

(defn cache-label-prop-in-mark-data
  [vega mark label-prop cache]
  (-> vega
    (ovu/validate-syms [] [mark])
    (util/assoc-in-with-kv-index [:marks [:name mark] :encode :enter cache :field] label-prop)))

(defn add-node-labels
  "Add labels to nodes in visualization. Uses label-prop as node label.

  Provides: sym"
  [vega nodes-mark label-prop]
  (let [sym   (ovu/prop-sym vega :labels nodes-mark)
        cache :cached_label]
    (-> vega
      (ovu/validate-syms [sym] [nodes-mark])
      (cache-label-prop-in-mark-data nodes-mark label-prop cache)
      (update :marks conj {:name   sym
                           :type   :text
                           :from   {:data nodes-mark}
                           :zindex 2
                           :encode {:enter  {:text  {:field cache}
                                             :align {:value :center}}
                                    :update {:x {:field :x}
                                             :y {:field :y}}}}))))

(def diff-x "(datum.tx - datum.sx)")
(def diff-y "(datum.ty - datum.sy)")
(def phi (format "atan2(%s, %s)" diff-y diff-x))

(defn set-angle
  [degrees]
  (format "%d + 180/PI * %s" degrees phi))

(defn midway-plus
  [plus axis]
  (format "%s + (datum.s%s + datum.t%s)/2" plus axis axis))

(defn add-formula-transform
  [vega sym prop expr]
  (-> vega
    (ovu/validate-syms [] [sym])
    (util/update-in-with-kv-index [:marks [:name sym] :transform] conj
      {:type :formula :as prop :expr expr})))

(defn set-link-marker-position
  [vega sym x-expr y-expr angle-expr]
  (let [f "%s * %s"]
    (-> vega
      (ovu/validate-syms [] [sym])
      (util/update-in-with-kv-index [:marks [:name sym] :encode :update] assoc
        :sx {:field "datum.source.x"}
        :sy {:field "datum.source.y"}
        :tx {:field "datum.target.x"}
        :ty {:field "datum.target.y"})
      (add-formula-transform sym :x x-expr)
      (add-formula-transform sym :y y-expr)
      (add-formula-transform sym :angle angle-expr))))

(defn polar-coord
  [axis r-f normal?]
  (format "%s(%s) * %s" (case axis
                          "x" (if normal? "sin" "cos")
                          "y" (if normal? "-cos" "sin"))
    phi (r-f axis)))

(defn add-link-labels
  "Add labels to links in visualization. Uses label-prop as node label

  Provides: sym"
  [vega links-mark label-prop & {:keys [extra-height]}]
  (let [sym              (ovu/prop-sym vega :labels links-mark)
        vega             (ovu/validate-syms vega [sym] [])
        extra-height-sym (ovu/prop-sym vega :extra_height sym)
        cache            :cached_label]
    (-> vega
      (ovu/validate-syms [] [links-mark])
      (ovu/add-range extra-height-sym extra-height)
      (cache-label-prop-in-mark-data links-mark label-prop cache)
      (update :signals conj {})
      (update :marks conj {:name   sym
                           :type   :text
                           :from   {:data links-mark}
                           :zindex 2
                           :encode {:enter  {:text  {:field cache}
                                             :align {:value :center}}
                                    :update {:extra_height {:signal extra-height-sym}}}})
      (set-link-marker-position sym
        (polar-coord "x" (partial midway-plus "datum.extra_height") true)
        (polar-coord "y" (partial midway-plus "datum.extra_height") true)
        (set-angle 0)))))

(defn add-link-directions
  [vega links-mark nodes-mark & {:keys [centered? extra-radius] }]
  (let [sym              (ovu/prop-sym vega :directions links-mark)
        vega             (ovu/validate-syms vega [sym] [])
        extra-radius-sym (ovu/prop-sym vega :extra_radius sym)
        rad-sym          (ovu/prop-sym vega :radius nodes-mark)
        r-off            (fn [axis]
                           (if centered?
                             (midway-plus "datum.extra_rad" axis)
                             (let [arrow-rad   "sqrt(datum.size)/2"
                                   path-length (format "sqrt(%s * %s + %s * %s)"
                                                 diff-x diff-x diff-y diff-y)]
                               (format "(%s - (%s + %s + %s)) + datum.s%s"
                                 path-length "datum.r"
                                 "datum.extra_rad" arrow-rad axis))))]
    (-> vega
      (ovu/validate-syms [] [rad-sym links-mark])
      (ovu/add-range extra-radius-sym  extra-radius)
      (update :marks conj {:name   sym
                           :type   :symbol
                           :from   {:data links-mark}
                           :zindex 2
                           :encode {:enter  {:x     0
                                             :y     0
                                             :shape {:value :arrow}}
                                    :update {:extra_rad {:signal extra-radius-sym }
                                             :r         {:signal rad-sym}}}})
      (set-link-marker-position sym
        (polar-coord "x" r-off false)
        (polar-coord "y" r-off false)
        (set-angle 90)))))
