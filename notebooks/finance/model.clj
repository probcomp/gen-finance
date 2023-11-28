^{:nextjournal.clerk/visibility {:code :hide}}
(ns finance.model
  {:nextjournal.clerk/toc true}
  (:require [emmy.clerk :as ec]
            [emmy.leva :as leva]
            [emmy.viewer :as ev]
            [gen.choicemap :as choicemap]
            [gen.distribution.kixi :as kixi]
            [gen.dynamic :as dynamic :refer [gen]]
            [gen.inference.importance :as importance]
            [gen.trace :as trace]
            [gen.generative-function :as gf]
            [nextjournal.clerk :as clerk]))

{::clerk/visibility {:code :hide :result :hide}}
(ec/install!)

(defn round [^double x]
  (Math/round x))

(defn revenue+cost-schema
  [data]
  {:schema "https://vega.github.io/schema/vega-lite/v5.json"
   :embed/opts {:actions false}
   :data {:values data}
   :width 650 :height 300
   :layer
   [{:mark :line
     :encoding {:x {:field :period :type "quantitative"}
                :y {:field :revenue :type "quantitative"}}}
    {:mark {:type :line :color :red}
     :encoding {:x {:field :period :type "quantitative"}
                :y {:field :total-cost :type "quantitative"}}}]})

(def revenue+cost-chart
  (comp clerk/vl revenue+cost-schema))

(defn format-money [value]
  (str (if (neg? value)
         "–$"
         "$")
       (format "%.2f" (double (Math/abs value)))))

(defn ->table [sim]
  (clerk/table
   (map (fn [m]
          (reduce (fn [m k] (update m k format-money))
                  m
                  [:ad-spend :cost-of-service :profit-per-user
                   :revenue :total-cost]))
        sim)))

(defn cpa
  "Cost per user acquisition."
  [cost users]
  (double
   (/ cost users)))

(defn total-value [simulation]
  (reduce (fn [value {:keys [revenue total-cost]}]
            (+ value (- revenue total-cost)))
          0.0
          simulation))

(def simulate-business
  (gen [{:keys [users cost] :or {users 0 cost 0}}
        {:keys [periods
                retention-mean
                cost-of-service
                revenue-per-paying
                free->pay
                ad-spend-increase
                viral-growth-kicker]
         :or   {retention-mean      0.8
                periods             20
                cost-of-service     0
                revenue-per-paying  0
                free->pay           0
                ad-spend-increase   0
                viral-growth-kicker 0}}]
       (let [retention-rate (dynamic/trace! :retention kixi/normal retention-mean 0.05)
             cost-per-user (dynamic/trace! :cost-of kixi/normal cost-of-service 0.01)
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
                              :cpa                     (cpa cost users)}
             simulation
             (take periods
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
                    initial))
             value (total-value simulation)]
         (dynamic/trace! :total-value kixi/normal value 0.001)
         simulation)))

(defn simulation-choices
  [{:keys [initial-data config]}]
  (repeatedly
   (:trials config 10)
   (fn []
     (choicemap/->map
      (trace/get-choices
       (gf/simulate simulate-business [initial-data config]))))))

(defn choices->scatterplot [data]
  {:schema "https://vega.github.io/schema/vega-lite/v5.json"
   :embed/opts {:actions false}
   :width 650 :height 300
   :data {:values data}
   :layer
   [{:mark :point
     :encoding {:x {:field :retention :type "quantitative"}
                :y {:field :total-value :type "quantitative"}}}
    {:mark {:type :point :color :red}
     :encoding {:x {:field :cost-of :type "quantitative"}
                :y {:field :total-value :type "quantitative"}}}]})

(defn value-histogram [data]
  {:schema "https://vega.github.io/schema/vega-lite/v5.json"
   :embed/opts {:actions false}
   :width 650 :height 300
   :data {:values data}
   :mark {:type :bar :color :blue}
   :encoding {:x {:bin true :field :total-value}
              :y {:aggregate "count"}}})

{::clerk/visibility {:code :show :result :show}}

;; ## Simulating a Business

;; `initial-data` contains entries for
;;
;; - `:users`: the starting number of users and
;; - `:cost`: the total amount of advertising spend allocated per time period.

(def initial-data
  {:users 5130
   :cost 33300.0})

;; This configuration holds the initial parameters of the simulation we'll run
;; below.
;;
;; Notice that each entry maps to a slider in the hovering control panel.
;;
;; - `:trials`: total number of simulations to generate
;; - `:periods`: number of time periods to simulate the business
;; - `:value-target`: the goal business value, used for importance sampling.
;; - `:n-samples`: number of samples used in importance sampling
;; - `:free->pay`: percentage of users that convert from free => paid each time period
;; - `:ad-spend-increase`: percentage increase in ad spending per period
;; - `:viral-growth-kicker`: ratio of new viral users to last time period's total new users
;; - `:retention-mean`: average user retention rate, constant per simulation
;; - `:cost-of-service`: total cost to serve a user
;; - `:revenue-per-paying`: revenue per paying customer per time period

(def config
  {:trials              10
   :periods             20
   :value-target        10000
   :n-samples           10
   :free->pay           10
   :ad-spend-increase   5
   :viral-growth-kicker 0.75
   :retention-mean      0.8
   :cost-of-service     0.1
   :revenue-per-paying  6.446})

;; `simulate-business` lets us simulate the history of a business with all of
;; the assumptions from `config`:

(def simulation
  (simulate-business initial-data config))

;; Let's look at the first 10 time periods of data in table form:

(->table (take 10 simulation))

;; Next, let's look at a chart of total revenue (in blue) and total costs (in
;; red) over time. For the business to be viable, the blue line needs to cross
;; above the red line.

(revenue+cost-chart simulation)

;; The **total value** of the business is the sum of `(- revenue cost)` for all
;; time periods. This particular business is worth the following:

(total-value simulation)

;; ## Simulating Many Trials
;;
;; The following charts are tied to the slider hovering over the page. Play with
;; the sliders and watch the charts update.
;;
;; The first chart shows the revenue+cost chart from above, but tied to the
;; sliders.
;;
;; The next two charts are generated by simulating many businesses and plotting aggregates.

;; The scatterplots show pairs of (random choice, total value) for each type of
;; random choice made by the model.
;;
;; The final chart shows a histogram of total business value generated by all
;; trials.

(def schema
  {"Simulation Params"
   (leva/folder
    {:trials       {:min 0 :max 1000 :step 5}
     :periods      {:min 1 :max 50 :step 1}
     :value-target {:min 0 :max 10000000 :step 1000}
     :n-samples    {:min 0 :max 100 :step 5}}
    {:order -1})

   "Business Config"
   (leva/folder
    {:retention-mean      {:min 0 :max 1 :step 0.01}
     :free->pay           {:min 0 :max 100 :step 0.01}
     :ad-spend-increase   {:min 0 :max 100 :step 0.01}
     :viral-growth-kicker {:min 0 :max 1 :step 0.01}
     :cost-of-service     {:min 0 :max 10 :step 0.01}
     :revenue-per-paying  {:min 0 :max 70 :step 0.01}})})

^{::clerk/visibility {:code :hide}}
(ev/with-let [!state config]
  [:<>
   (leva/controls
    {:atom   !state
     :schema schema})
   (list 'let ['config {:initial-data `initial-data
                        :config       (list 'deref !state)}
               'data  (list `simulation-choices 'config)
               'trial (list `simulate-business
                            `initial-data
                            (list 'deref !state))]
         [:<>
          ['nextjournal.clerk.render/render-vega-lite
           (list `revenue+cost-schema 'trial)]
          ['nextjournal.clerk.render/render-vega-lite
           (list `choices->scatterplot 'data)]
          ['nextjournal.clerk.render/render-vega-lite
           (list `value-histogram 'data)]])])

;; ## Inference
;;
;; Not a great visualization, but here's a start:

(defn do-inference
  [{:keys [initial-data config]}]
  (let [{:keys [trials value-target n-samples]} config
        infer (fn []
                (-> (importance/resampling simulate-business
                                           [initial-data config]
                                           {:total-value value-target}
                                           n-samples)
                    (:trace)
                    (trace/get-choices)
                    (choicemap/->map)))]
    (repeatedly trials infer)))

^{::clerk/visibility {:code :hide}}
(ev/with-let [!state config]
  [:<>
   (leva/controls
    {:atom !state :schema schema})
   ['nextjournal.clerk.render/render-vega-lite
    (list `choices->scatterplot
          (list `do-inference
                {:initial-data `initial-data
                 :config  (list 'deref !state)}))]])
