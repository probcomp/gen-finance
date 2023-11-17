^{:nextjournal.clerk/visibility {:code :hide}}
(ns finance.slim
  {:nextjournal.clerk/toc true}
  (:require [emmy.clerk :as ec]
            [emmy.leva :as leva]
            [gen.choicemap :as choicemap]
            [gen.distribution.kixi :as kixi]
            [gen.dynamic :as dynamic :refer [gen]]
            [gen.trace :as trace]
            [gen.generative-function :as gf]
            [nextjournal.clerk :as clerk]))

{::clerk/visibility {:code :hide :result :hide}}

(ec/install!)

(defn round [^double x]
  (Math/round x))

(defn scan [fold xs]
  (->> (reductions fold (fold) xs)
       (rest)
       (map fold)))

(defn overlay
  [layers]
  (clerk/vl
   {:schema "https://vega.github.io/schema/vega-lite/v5.json"
    :embed/opts {:actions false}
    :layer layers}))

(defn user-growth-chart [data]
  {:schema "https://vega.github.io/schema/vega-lite/v5.json"
   :embed/opts {:actions false}
   :data {:values data}
   :width 650 :height 300
   :layer
   [{:mark :line
     :encoding {:x {:field :period :type "quantitative"}
                :y {:field :cumulative-users :type "quantitative"}}}
    {:mark {:type :line :color :red}
     :encoding {:x {:field :period :type "quantitative"}
                :y {:field :cumulative-paying-users :type "quantitative"}}}]})

(defn revenue+cost-chart
  [data]
  {:schema "https://vega.github.io/schema/vega-lite/v5.json"
   :embed/opts {:actions false}
   :data {:values data}
   :width 650 :height 300
   :layer
   [{:mark :line
     :encoding {:x {:field :period :type "temporal"}
                :y {:field :revenue :type "quantitative"}}}
    {:mark {:type :line :color :red}
     :encoding {:x {:field :period :type "temporal"}
                :y {:field :total-cost :type "quantitative"}}}]})

(defn format-money [value]
  (clerk/html
   [:div.text-right.tabular-nums
    (when (neg? value)
      [:span.text-red-500 "–"])
    [:span.text-slate-400 "$"]
    [:span (format "%.2f" (double (Math/abs value)))]]))

(defn cpa
  "Cost per user acquisition."
  [cost users]
  (double
   (/ cost users)))


{::clerk/visibility {:code :show :result :show}}

;; ## Media Buys

(defn buy->count
  [{:keys [cpm cpc impressions clickthrough signup]
    :or   {cpm 0 cpc 0 impressions 0 clickthrough 0 signup 0}}]
  (let [clicks (round (* impressions (/ clickthrough 100)))
        users  (round (* clicks (/ signup 100)))
        cost   (+ (* cpm (/ impressions 1000))
                  (* cpc clicks))]
    {:users  users
     :cost   cost
     :clicks clicks}))

(defn media-buys->counts
  [buys]
  (transduce
   (map buy->count)
   (fn
     ([] {:users 0 :cost 0 :clicks 0 :cpa ##Inf})
     ([m] (assoc m :cpa (cpa (:cost m) (:users m))))
     ([l r]
      (merge-with + l r)))
   buys))

(def initial-business-metrics
  "This data is required input for the remaining charts."
  (media-buys->counts
   [{:cpc 1.00
     :impressions 1000000
     :clickthrough 3.13
     :signup 10}
    {:cpm 1.00
     :impressions 2000000
     :clickthrough 0.5
     :signup 20}]))

(def simulate-business
  (gen [{:keys [users cost] :or {users 0 cost 0}}
        {:keys [retention-mean
                cost-of-service
                revenue-per-paying
                free->pay
                ad-spend-increase
                viral-growth-kicker]
         :or   {retention-mean      0.8
                cost-of-service     0
                revenue-per-paying  0
                free->pay           0
                ad-spend-increase   0
                viral-growth-kicker 0}}]
    (let [retention-rate  (dynamic/trace! :retention kixi/normal retention-mean 0.05)
          cost-per-user   (dynamic/trace! :cost-of kixi/normal cost-of-service 0.01)
          pay-rate        (/ free->pay 100)
          paying          (round (* pay-rate users))
          revenue         (* revenue-per-paying paying)
          init-cpa        (cpa cost users)
          spend-rate      (inc (/ ad-spend-increase 100.0))
          cost-of-service (* cost-per-user users)
          initial         {:period                  0
                           :ad-spend                cost
                           :total-new-users         users
                           :virally-acquired-users  0
                           :bought-users            users
                           :paying-users            paying
                           :revenue                 revenue
                           :cost-of-service         cost-of-service
                           :total-cost              (+ cost cost-of-service)
                           :profit-per-user         (/ (- revenue cost cost-of-service)
                                                       paying)
                           :cumulative-users        users
                           :cumulative-paying-users paying
                           :cpa                     (cpa cost users)}]

      (iterate
       (fn [prev]
         (let [new-spend        (* spend-rate (:ad-spend prev))
               viral-users      (round (* viral-growth-kicker (:total-new-users prev)))
               bought-users     (round (/ new-spend init-cpa))
               total-new-users  (+ bought-users viral-users)
               new-paying-users (round (* pay-rate total-new-users))
               users-sum        (round
                                 (+ (* retention-rate
                                       (:cumulative-users prev))
                                    total-new-users))
               paying-sum       (round
                                 (+ (* retention-rate
                                       (:cumulative-paying-users prev))
                                    new-paying-users))
               cost-of-service  (* cost-per-user users-sum)
               total-cost       (+ new-spend cost-of-service)
               revenue          (* revenue-per-paying paying-sum)]
           {:period                  (inc (:period prev))
            :ad-spend                new-spend
            :total-new-users         total-new-users
            :virally-acquired-users  viral-users
            :bought-users            bought-users
            :paying-users            new-paying-users
            :revenue                 revenue
            :cost-of-service         cost-of-service
            :total-cost              total-cost
            :profit-per-user         (/ (- revenue total-cost)
                                        new-paying-users)
            :cumulative-users        users-sum
            :cumulative-paying-users paying-sum
            :cpa                     (cpa new-spend total-new-users)}))
       initial))))

(def config
  {:retention           0.8
   :free->pay           10
   :ad-spend-increase   5
   :viral-growth-kicker 0.75
   :retention-mean      0.8
   :cost-of-service     0.1
   :revenue-per-paying  6.446})

(defn total-value [simulation]
  (transduce (map (fn [{:keys [revenue total-cost]}]
                    (- revenue total-cost)))
             +
             simulation))



(defn run-simulations [periods trials]
  (let [traces (repeatedly
                trials
                #(gf/simulate simulate-business [initial-business-metrics config]))]
    (map (fn [trace]
           (let [sim (take periods (trace/get-retval trace))]
             (-> (choicemap/->map
                  (trace/get-choices trace))
                 (assoc :total-value (total-value sim)))))
         traces)))

(defn total-value-vs-k-scatter [k]
  (clerk/vl
   {:schema "https://vega.github.io/schema/vega-lite/v5.json"
    :embed/opts {:actions false}
    :data {:values (run-simulations 20 10)}
    :mark :point
    :encoding {:x {:field k :type "quantitative"}
               :y {:field :total-value :type "quantitative"}}}))

(clerk/row
 (total-value-vs-k-scatter :retention)
 (total-value-vs-k-scatter :cost-of))

(def simulation
  (simulate-business initial-business-metrics config))

(let [data (take 10 simulation)]
  (clerk/col
   (clerk/table data)
   (clerk/vl
    (user-growth-chart data))))

;; Here are the final rows of this part of the sheet:

;; TODO scatterplot of total value vs one of the random parameters
;;
;; TODO (also maybe histogram of total value)
;;
;; TODO next - use importance sampling to enforce a complex constraint... this
;; parameter is less than 10, total value is greater than 100. Find values of
;; other parameters that satisfy these sets of constraints.
;;
;; TODO we want total value in the trace. do it by making it the mean of a
;; normal with a slider for a (small) variance. Required for importance
;; sampling. Try ~1000 samples, make broad priors at first.

(let [periods 20
      trials  10]
  (overlay
   (mapv
    (fn [data]
      (revenue+cost-chart (take periods data)))
    (repeatedly trials #(simulate-business initial-business-metrics config)))))

(let [data (take 20 simulation)]
  (clerk/col
   (clerk/table data)
   (clerk/vl
    (revenue+cost-chart data))))
