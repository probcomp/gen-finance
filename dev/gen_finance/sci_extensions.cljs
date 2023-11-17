(ns gen-finance.sci-extensions
  "SCI environment extensions for Gen + Emmy."
  (:require [emmy.viewer.sci]
            [goog.string :as gs]
            [gen.sci]
            [sci.core :as sci]
            [sci.ctx-store]))

(emmy.viewer.sci/install!)
(gen.sci/install!)
(sci.ctx-store/swap-ctx!
 sci/merge-opts
 {:namespaces
  {'clojure.core {'format gs/format}}})
