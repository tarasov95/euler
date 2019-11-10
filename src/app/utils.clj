(ns app.utils
  (:require [app.prob3 :as p3]) )

(defn raw-factors [n]
  (p3/list-prime-factors p3/list-of-dividers n))

(defn map-of-factors [rg]
  (reduce (fn [r e] (assoc r (last e) (second e))) {} rg))

(def prime-factors (comp map-of-factors raw-factors))


(defn mult-factors [mp1 mp2]
  (merge-with + mp1 mp2))
