^{:nextjournal.clerk/visibility {:code :hide}}
(ns finance.model
  {:nextjournal.clerk/toc true}
  (:require [emmy.clerk :as ec]
            [emmy.leva :as leva]
            [emmy.viewer :as ev]
            [gen.distribution.kixi :as kixi]
            [gen.dynamic :as dynamic :refer [gen]]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:code :hide :result :hide}}
(ec/install!)

(defn cpa
  [cost users]
  (if (zero? users)
    ##Inf
    (double
     (/ cost users))))

(defn round [^double x]
  (Math/round x))

(defn process-row
  [{:keys [cpm cpc impressions clickthrough signup]
    :or   {cpm 0 cpc 0 impressions 0 clickthrough 0 signup 0}}]
  (let [clicks (round (* impressions (/ clickthrough 100)))
        users  (round (* clicks (/ signup 100)))
        cost   (+ (* cpm (/ impressions 1000))
                  (* cpc clicks))]
    {:users  users
     :cost   cost
     :clicks clicks}))

(defn sheet-one
  "Takes a sequence of maps with the following entries, all optional:

   :cpm - cost per thousand impressions
   :cpc - cost per click
   :impressions - total number of impressions
   :clickthrough - percentage in the interval [0,100] of clicks per impression
   :signup - percentage in the interval [0,100] of signups per click


  Returns a map of

  ```clojure
  {:users  <total users signed up>
   :cost   <total cost>
   :clicks <total clicks>
   :cpa    <total cost per user signup>}
  ```
  "
  [& ms]
  (transduce
   (map process-row)
   (fn
     ([] {:users 0 :cost 0 :clicks 0 :cpa ##Inf})
     ([m] (assoc m :cpa (cpa (:cost m) (:users m))))
     ([l r]
      (merge-with + l r)))
   ms))

(defn funnel
  "Takes 'spend metrics', the output of sheet 1, with these keys:

  :users - initial number of registered users
  :cost  - total ad spend (cpa == users / cost)

  And config:

  :free->pay - percentage of free converting to paid / time period
  :ad-spend-increase - percentage increase in ad spend / time period

  TODO alternatively we could provide CPA and users, and not total cost? but
  that's awkward with how we're doing the increase i ad spend.

  TODO the sheet also had these...
  :invite-conversion-rate
  :avg-invites
  "
  ([metrics] (funnel metrics {}))
  ([{:keys [users cost] :or {cost 0 users 0}}
    {:keys [free->pay
            ad-spend-increase
            viral-growth-kicker]
     :or {free->pay 0 ad-spend-increase 0 viral-growth-kicker 0}}]
   (let [pay-rate   (/ free->pay 100)
         paying     (Math/round ^double (* pay-rate users))
         init-cpa   (cpa cost users)
         spend-rate (inc (/ ad-spend-increase 100.0))
         increment  (fn [{:keys [period total-new-users ad-spend
                                cumulative-users
                                cumulative-paying-users]}]
                      (let [spend  (* ad-spend spend-rate)
                            viral  (Math/round ^double (* viral-growth-kicker total-new-users))
                            bought (Math/round ^double (/ spend init-cpa))
                            total  (+ bought viral)
                            paying (Math/round ^double (* pay-rate total))]
                        {:period (inc period)
                         :virally-acquired-users viral
                         :bought-users bought
                         :ad-spend spend
                         :total-new-users total
                         :paying-users paying
                         :cumulative-users (+ cumulative-users total)
                         :cumulative-paying-users (+ cumulative-paying-users paying)
                         :cpa (cpa spend total)}))]
     (iterate increment
              {:period 0
               :virally-acquired-users 0
               :bought-users users
               :ad-spend cost
               :total-new-users users
               :paying-users paying
               :cumulative-users users
               :cumulative-paying-users paying
               :cpa (cpa cost users)}))))

(defn render-metrics
  [n data]
  (clerk/vl
   {:schema "https://vega.github.io/schema/vega-lite/v5.json"
    :embed/opts {:actions false}
    :data {:values (take n data)}
    :width 650 :height 300
    :layer
    [{:mark :line
      :encoding {:x {:field :period :type "temporal"}
                 :y {:field :cumulative-users :type "quantitative"}}}
     {:mark {:type :line :color :red}
      :encoding {:x {:field :period :type "temporal"}
                 :y {:field :cumulative-paying-users :type "quantitative"}}}]}))

;; Tying it all together:

(ev/with-let [!state {:clickthrough 3.13
                      :retention 0.8
                      :free->pay 10
                      :ad-spend-increase 5
                      :viral-growth-kicker 0.75}]
  [:<>
   (leva/controls
    {:atom !state
     :folder {:name "Config"}
     :schema
     {:clickthrough        {:min 1 :max 10 :step 0.01}
      :retention           {:min 0 :max 1 :step 0.01}
      :free->pay           {:min 0 :max 100 :step 0.01}
      :ad-spend-increase   {:min 0 :max 100 :step 0.01}
      :viral-growth-kicker {:min 0 :max 1 :step 0.01}}})
   (list `sheet-one
         {:cpc 1.00
          :impressions 1000000
          :clickthrough (ev/get !state :clickthrough)
          :signup 10}
         {:cpm 1.00
          :impressions 2000000
          :clickthrough 0.5
          :signup 20})])
