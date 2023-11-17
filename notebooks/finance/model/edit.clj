(ns finance.model.edit
  "Editable version of the `model` notebook."
  #:nextjournal.clerk
  {:no-cache true
   :visibility {:code :hide}
   :doc-css-class [:overflow-hidden :p-0]}
  (:require [nextjournal.clerk :as clerk]))

(clerk/with-viewer
  {:render-fn 'nextjournal.clerk.render.editor/view
   :transform-fn clerk/mark-presented}
  (slurp "notebooks/finance/model.clj"))
