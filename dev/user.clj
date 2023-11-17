(ns user
  (:require [gen.clerk :as clerk]))

(def index
  "notebooks/finance/intro.clj")

(def defaults
  {:index index
   :cljs-namespaces '[gen-finance.sci-extensions]})

(def serve-defaults
  (assoc defaults
         :port 7777
         ;; :watch-paths ["notebooks"]
         :browse? true))

(def static-defaults
  (assoc defaults
         :paths ["notebooks/**.clj"]
         :git/url "https://github.com/probcomp/gen-finance"))

(defn serve!
  "Alias of [[gen.clerk/serve!]] with [[defaults]] supplied as default arguments.

  Any supplied `opts` overrides the defaults."
  ([] (serve! {}))
  ([opts]
   (clerk/serve!
    (merge serve-defaults opts))))

(def ^{:doc "Alias for [[gen.clerk/halt!]]."}
  halt!
  clerk/halt!)

(defn build!
  "Alias of [[gen.clerk/build!]] with [[static-defaults]] supplied as default
  arguments.

  Any supplied `opts` overrides the defaults."
  [opts]
  (clerk/build!
   (merge static-defaults opts)))
