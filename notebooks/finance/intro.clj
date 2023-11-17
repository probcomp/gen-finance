^{:nextjournal.clerk/visibility {:code :hide}}
(ns finance.intro
  {:nextjournal.clerk/toc true}
  (:require [emmy.clerk :as ec]
            [emmy.leva :as leva]
            [gen.distribution.kixi :as kixi]
            [gen.dynamic :as dynamic :refer [gen]]
            [nextjournal.clerk :as clerk]))

^{::clerk/visibility {:code :hide :result :hide}}
(ec/install!)

^{::clerk/visibility {:code :hide :result :hide}}
(defn format-money [value]
  (clerk/html
   [:div.text-right.tabular-nums
    (when (neg? value)
      [:span.text-red-500 "–"])
    [:span.text-slate-400 "$"]
    [:span (format "%.2f" (double (Math/abs value)))]]))

;; # Welcome!
;;
;; This is a port of this [blog
;; post](https://andrewchen.com/how-to-create-a-profitable-freemium-startup-spreadsheet-model-included/)
;; from Andrew Chen.

;; Here are the questions this post (and the spreadsheet) is meant to answer:

;; - What are the key factors that drive freemium profitability?
;; - How do freemium businesses acquire customers?
;; - What are the drivers of customer lifetime value?
;; - How do all these variables interact?
;;
;;
;; ## Summary
;;
;; To become profitable using a freemium business model, this simple equation
;; must hold true:
;;
;; **Lifetime value > Cost per acquisition + Cost of service (paying & free)**
;;
;;
;; ## User Acquisition
;;
;; The first tab in the spreadsheet covers the issue of paid user acquisition –
;; many subscription businesses mostly rely on AdWords and ad network buys in
;; order to acquire users. For freemium businesses, particularly ones that are
;; social apps, there’s often a word of mouth or viral component, which we’ll
;; cover in a second.

;; At a high level, here are some of the things you’ll want to track:

;; * How are you paying for traffic? (CPM/CPA/CPC)
;; * What do the intermediate metrics look like? (impressions/CTR/etc)
;; * How does your signup funnel perform?
;; * How much are you spending for the users you end up registering?

;; Basically, you end up with a media buying matrix that looks something like this:

^{::clerk/visibility {:code :hide}}
(clerk/table
 (clerk/use-headers
  [["Source" "Ads bought" "CTR" "Clicks" "Signup %" "Upload pic" "Users" "Cost" "CPA"]
   ["Google" "1M" "0.50%" 5000 "20%" "50%" 500 "$5,000.00" "$10.00"]
   ["Ad.com" "20M" "0.10%" 20000 "10%" "50%" 1000 "$20,000.00" "$20.00"]]))

;; and these are some factors worth thinking about, in terms of increasing or
;; decreasing the cost per acquisition (CPA):

^{::clerk/visibility {:code :hide}}
(clerk/table
 (clerk/use-headers
  [["Type" "Options" "Importance"]
   ["Source of traffic" "Ad networks, publishers" "++"]
   ["Cost model" "CPM, CPC, CPA" "+"]
   ["User requirements" "Install, browser plug-in, Flash" "+++++"]
   ["Audience and theme" "Horizontal vs vertical" "++"]
   ["Funnel design" "Landing page, length, fields" "+++"]
   ["Viral marketing" "Facebook, Opensocial, email" "+++++"]
   ["A/B testing process" "None, homegrown, Google" "+++++"]]))

;; ### Model
;;
;; This first page encodes the following ideas:

(defn cpa
  [cost users]
  (if (zero? users)
    ##Inf
    (double
     (/ cost users))))

(defn round [^double x]
  (Math/round x))

(defn ^:no-doc process-row
  "Processes a single row for [[sheet-one]]."
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

^::clerk/sync
(defonce !state
  (atom {:clickthrough 3.13
         :retention 0.8
         :free->pay 10
         :ad-spend-increase 5
         :viral-growth-kicker 0.75}))

(leva/controls
 {:atom '!state
  :folder {:name "Config"}
  :schema
  {:clickthrough        {:min 1 :max 10 :step 0.01}
   :retention           {:min 0 :max 1 :step 0.01}
   :free->pay           {:min 0 :max 100 :step 0.01}
   :ad-spend-increase   {:min 0 :max 100 :step 0.01}
   :viral-growth-kicker {:min 0 :max 1 :step 0.01}}})

(def spend-metrics
  "This data is required input for the remaining charts."
  (sheet-one
   {:cpc 1.00
    :impressions 1000000
    :clickthrough (:clickthrough @!state)
    :signup 10}
   {:cpm 1.00
    :impressions 2000000
    :clickthrough 0.5
    :signup 20}))

;; TODO - deeper refactoring to expose the conceptual model. There is a
;; generator for events in the world (user joined, user clicked, etc)
;;

;; - TODO - hook up leva sliders, make this more reactive
;; - TODO - make these generative

;; ## Funnel

;; Once you get your users registered onto the site, then there’s the question
;; of how convert to paying customers, and whether there are any viral effects.
;; The model covered in the spreadsheet has a separate tab, called “Funnel”
;; which covers these issues.

;; At a high level, there’s what is happening:

;; * Each time period, a bunch of newly registered users come in (both acquired through ads or through viral marketing)
;; * Some % of these users convert into paying users
;; * Some % of these users then send off viral invites
;; * Revenue is generated by building up a base of paying users
;; * Cost is generated through building up a base of active users (paying or not!)
;;
;; To me, this tab captures the “art” side of building a freemium business.
;; Persuading peopleto pay for your service and invite their friends requires
;; creativity, product design, and lots of metrics. Josh Kopelman of First Round
;; Capital had a great tweet recently on this topic where he says:
;;
;; > @joshk: Too many freemium models have too much free and not enough mium

;; As Josh notes, the key is to create the right mix of features to segment out
;; the people who are willing to pay, but without alienating the users who make
;; up your free audience. Do it right, and your conversion rates might be as
;; high as 20%. Do it wrong, and your LTV gets very close to zero. This is why
;; premium features have to be built into the core of a freemium business,
;; rather than added in at the end. You want to be right at the balance between
;; free and ‘mium!

;; Just remember that during the time period that it takes you to figure out
;; your funnel, viral loop, and everything else, all the free users you’re
;; building up create cost in your system.

;; Businesses that aren’t eyeball businesses shouldn’t act like eyeball
;; businesses :-)

;; Anyway, the product design issue (and resultant conversion rates) are a a
;; deep topic, and here are some other related posts (by others and myself):

;; - Thoughts on free powered business models (Charles Hudson)
;; - Casual MMOs get between 10-25% of users to pay (Nabeel Hyatt)
;; - Successful MMOGs can see $1-$2 in monthly ARPU (Jeremy Liew)
;; - Bridging your traffic engine with your revenue engine
;; - What’s your viral loop? Understanding your engine of adoption

;; ### Model

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
      :encoding {:x {:field :period :type "quantitative"}
                 :y {:field :cumulative-users :type "quantitative"}}}
     {:mark {:type :line :color :red}
      :encoding {:x {:field :period :type "quantitative"}
                 :y {:field :cumulative-paying-users :type "quantitative"}}}]}))

(def funnel-data
  (funnel spend-metrics @!state))

(let [n 10]
  (clerk/col
   (clerk/table
    (take n funnel-data))
   (render-metrics n funnel-data)))

;; ## User retention

;; Of course, it’s not enough to just acquire paying users, you need to retain
;; them. If you have a super high churn rate, then at best you’ll be stuck at a
;; revenue treadmill (doing lots of work but flat revenue and no profitability).
;; At worse, it’s easy to lose a ton of money, if the CPA exceeds the LTV. I
;; wrote about this topic earlier in my essay When and why do Facebook apps jump
;; the shark (which also has a spreadsheet).
;;
;; How sensitive are retention numbers on lifetime value? Here’s a quick thought
;; experiment: Lifetime value is the sum of the revenue that a user might
;; generate from their first time period to when they quit the service. Think of
;; it as an infinite sum that looks like:
;;

^{::clerk/visibility {:code :hide}}
(clerk/tex
 "LTV = rev + rev \\cdot R + rev \\cdot R^2 + rev \\cdot R^3 + \\ldots")

;; where $rev$ is the revenue that a user produces during a time period, and $R$
;; is the retention rate between time periods.
;;
;; You can simplify this, based on the [magic of infinite
;; series](http://en.wikipedia.org/wiki/List_of_mathematical_series):
;;

^{::clerk/visibility {:code :hide}}
(clerk/tex
 "LTV = \\frac{1}{1-R} \\cdot rev")

;; So let’s say that you make $1 per time period, and you have 1000 paying
;; users. Let’s compare the difference between a 50% retention rate and a 75%
;; retention rate:
;; At a 50% retention rate:
;;
;; LTV = 1/(1-0.5) * $1 * 1000 = $2000
;;
;; At a 75% retention rate:
;;
;; LTV = 1/(1-0.75) * $1 * 1000 =  $4000
;;
;; This means that in this case, by increasing your retention rate by
;; half (relatively speaking), you actually DOUBLE your revenue. And even more
;; when you reach “killer app” status and attain retention rates around 90%.
;; This is a big lever.

;; At a 90% retention rate:
;;
;; LTV = 1/(1-0.90) * $1 * 1000 = $10,000
;;
;; Note that retention rates are generally not fixed numbers – they usually get
;; better the longer a cohort of users stays with you! I’m using a fixed
;; retention number to set a lower bound, and for mathematical simplicity.
;;
;; OK, so the biggest factors affecting retention boil down to three things:
;;
;; * Product design
;; * Notifications (optimize them, of course)
;; * In success cases, saturation effects
;;
;; For more reading on product design, I’d recommend Designing Interactions from
;; IDEO. For notifications, there’s been a lot of great work in the database and
;; catalog marketing world, for example Strategic Database Marketing. Tesco,
;; Harrah’s, and Amazon are all companies well-known for their strategic use of
;; personalization and customer interaction. For saturation effects, as
;; previously mentioned, my old-ish article When and why do Facebook apps jump
;; the shark.
;;
;; ### Model

(defn retention-fold
  "Given a single decay rate, returns a fold:

  - 0 arity returns [], starting accumulation
  - 1-arity sums all cohort values for this time period
  - 2-arity decays out existing cohorts and adds a new, non-decayed cohort"
  [rate]
  (fn
    ([] [])
    ([v] (reduce + v))
    ([v amt]
     (-> (mapv (fn [x] (Math/round ^double (* x rate))) v)
         (conj amt)))))

(defn scan
  "Given a fold and a sequence, returns a sequence of all intermediate states seen
  during the fold."
  [fold xs]
  (->> (reductions fold (fold) xs)
       (rest)
       (map fold)))

(defn retention
  "Accepts a decay rate and key to query, and returns a function of a sequence of
  funnel data."
  [rate k]
  (fn [funnel-seq]
    (scan ((map k)
           (retention-fold rate))
          funnel-seq)))

;; Here are the final rows of this part of the sheet:

(defn revenue-per-paying
  "Takes a map of percentage => average revenue per customer for that bucket,
  returns the average total revenue per paying customer."
  [percent->rev]
  (transduce (map (fn [[percent avg-rev]]
                    (* percent avg-rev)))
             +
             percent->rev))

(def process-retention
  (gen [{:keys [retention-mean
                cost-of-service
                revenue-per-paying]
         :or   {retention-mean     0
                cost-of-service    0
                revenue-per-paying 0}}
        funnel-seq]
    (let [retention-rate  (dynamic/trace! :retention kixi/normal retention-mean 0.1)
          cost-of-service (dynamic/trace! :cost-of kixi/normal cost-of-service 0.1)]
      (map (fn [{:keys [period ad-spend]} paying total-new]
             (let [cost-of-service (* cost-of-service total-new)
                   revenue         (* revenue-per-paying paying)]
               {:period          period
                :ad-spend        ad-spend
                :paying-users    paying
                :total-new-users total-new
                :revenue         revenue
                :cost-of-service cost-of-service
                :total-cost      (+ ad-spend cost-of-service)
                :profit-per-user (/ (- revenue ad-spend cost-of-service)
                                    paying)}))
           funnel-seq
           ((retention retention-rate :paying-users) funnel-seq)
           ((retention retention-rate :total-new-users) funnel-seq)))))

(defn render-retention-spec
  [n data]
  {:schema "https://vega.github.io/schema/vega-lite/v5.json"
   :embed/opts {:actions false}
   :data {:values (take n data)}
   :width 650 :height 300
   :layer
   [{:mark :line
     :encoding {:x {:field :period :type "temporal"}
                :y {:field :revenue :type "quantitative"}}}
    {:mark {:type :line :color :red}
     :encoding {:x {:field :period :type "temporal"}
                :y {:field :total-cost :type "quantitative"}}}]})

(def render-retention
  (comp clerk/vl render-retention-spec))

(def retention-mean
  (:retention @!state))

(def revenue-per-paying-user
  (revenue-per-paying
   {0.1 25
    0.4 7.99
    0.5 1.50}))

(def retention-config
  {:retention-mean     retention-mean
   :cost-of-service    0.1
   :revenue-per-paying revenue-per-paying-user})

(def retention-data
  "decayed out users.."
  (process-retention retention-config funnel-data))

(defn overlay
  [layers]
  (clerk/vl
   {:schema "https://vega.github.io/schema/vega-lite/v5.json"
    :embed/opts {:actions false}
    :layer layers}))

;; TODO "simulate business" instead of "process retention", etc etc
;;
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

(overlay
 (mapv (fn [data]
         (render-retention-spec 10 data))
       (repeatedly 10 #(process-retention retention-config funnel-data))))

(let [n 20]
  (clerk/col
   (clerk/table
    (take n retention-data))
   (render-retention n retention-data)))

;; Questions
;;
;; Alternatively we need to get lifetime value in here somehow, based on the
;; discussion above. Or is that in the final one?
;;
;; ## Cashflow (and ad-reinvestment)

;; The tab “cashflow” in the spreadsheet captures a couple different issues:
;;
;; * Paid user acquisition is usually an upfront expense, whereas the revenue
;;   comes in over time
;; * Your revenue per paying user depends on a mix of revenue sources
;; * You pay a “cost of service” across all users, whether they are paying or
;;   not – be careful that this cost of service is not too high!!

;; Some more detail on the above:

;; In a model with paid user acquisition, it takes time to break even. You pay
;; for a user upfront, but then the revenue stream trickles in over several time
;; periods. As a result, you tend to be cashflow negative for some number of
;; time periods, and which then goes positive later. This effect is compounded
;; further if your model specifically depends on viral acquisition, because you
;; don’t get significant users in virally until your userbase becomes large.

;; This is why you get a graph like this, where you’re unprofitable for a while,
;; then break even:

;; Note that it’s also VERY possible that they never cross, and the entire
;; business is unprofitable. Just play around with the numbers in the
;; spreadsheet and you can see how easy it is to happen!


;; In terms of average revenue per paying customer, what you typically find is
;; that your customer base is made up of multiple segments. You can price them
;; differently through different tiers of subscription (Free versus Pro versus
;; Business) or with Pay-as-you-go or with many other models.


;; Ultimately you can roll this all up into a single number, which is referred
;; to in the spreadsheet as revenue per paying customer. You can also divide the
;; revenue by the number of total users (paying or not) in order to get the
;; average revenue per user (ARPU).


;; As for the cost of service, your mileage will vary. The main thing is, try
;; not to do anything too expensive for free users! After all, given that
;; typical conversion rates are <10%, and subscription services are typically
;; <$20/month, the following thought experiment is insightful:
;;
;; * Out of 1000 users, let’s say 50 pay $10/month. This generates $500/month
;; * This means that the costs must not exceed $500/month for 1000 users, or $0.50/user
;; * Plus then you have to factor in the acquisition cost! (Probably a couple
;;   bucks per user, so thousands of bucks per 1000 users).

;; ### Model

;; TODO table viewer composition... https://clojurians.slack.com/archives/C035GRLJEP8/p1688319224468099?thread_ts=1688297737.386799&cid=C035GRLJEP8

;; TODO get the money formatting back on the tables.

;; TODO make a graph of ad spend vs revenue, get my vega-lite going.

;; ## Lifetime value

;; And finally, the last tab on the spreadsheet calculates lifetime value.
;; Basically you figure out the number of payments that a paying user will
;; generate over their lifetime, referred to in the model as “user periods.” (I
;; arbitrarily took this out to 20 time periods, but you can do something
;; different) This is then multiplied by revenue per paying user, to get the
;; total dollar figure generated.

;; NOTE smiling that he doesn't use the infinite series thing from above... but
;; we have to ditch that anyway if we want to condition on the actual chance
;; that users stuck around per time period.

;; More important for the paid acquisition model is to do the LTV calculation
;; not for paying users, but for all registered users (paying or free). Doing
;; this then lets you figure out if you can profitably arbitrage traffic via ad
;; buying. This is done using the same method detailed in the above paragraph,
;; but using total user numbers rather than just paying users. Then you compare
;; this LTV number with the effective LTV that you get from buying users and
;; then factoring in their viral effects (as shown in the Funnel tab).

;; ### Model

;; SO for this last one... with retention rate, I added everything up across all
;; cohorts. But this wants us to do something different.

(defn ltv [revenue-per retention-rate]
  (/ revenue-per (- 1.0 retention-rate)))

(let [xs (funnel
          {:users                  5130
           :cost                   33300
           :free->pay              10
           :ad-spend-increase      5
           :viral-growth-kicker    0.75
           :invite-conversion-rate 15
           :avg-invites            5})
      periods 20
      periods-xs          (take periods xs)
      revenue-per         (revenue-per-paying
                           {0.1 25
                            0.4 7.99
                            0.5 1.50})
      retention-rate      0.8
      ltv (ltv revenue-per retention-rate)]
  (clerk/table
   {:ltv-paying (into [] (repeat periods (format-money ltv)))

    ;; TODO this is just LTV * retention rate for that time period. But this
    ;; maybe makes sense to tweak because again we might want to condition on
    ;; real data.
    :ltv-all (mapv (fn [{:keys [paying-users total-new-users]}]
                     (format-money
                      (* ltv (/ paying-users total-new-users))))
                   periods-xs)}))

;; ## Model improvements

;; Of course there are tons of things in this model of freemium businesses that ought to be improved!
;;
;; Benchmarks of real world data for comparison

;; * Benchmarks of real world data for comparison
;; * More granularity for user acquisition for affiliate versus ad buys versus other
;; * Saturation rates in the viral model
;; * Better model for retention rate other than one fixed number
;; * More sophisticated accounting of cost per user (infrastructure/employees/etc.)
;; * Model in multiple revenue sources including transaction fees, for Paypal versus Offerpal versus In-store cards versus mobile
;; * Better intelligence around ad-buying, including ramping up when profitable, slowing down when unprofitable

;; etc.

;; ### Clerk Improvements
;;
;; - TODO note that without `clerk/tex`, with double-dollars, the toc breaks.
;; - here's the video: https://www.youtube.com/watch?v=ztl8eBwq24k
;; - mindmap https://media.assembly.org/jussi/blog/vgsummit2008-metrics.pdf
