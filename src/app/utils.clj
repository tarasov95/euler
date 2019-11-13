(ns app.utils
  (:require [app.prob3 :as p3]) )

(defn raw-factors [n]
  (p3/list-prime-factors p3/list-of-dividers n))

(defn map-of-factors [rg]
  (reduce (fn [r e] (assoc r (last e) (second e))) {} rg))

(def prime-factors (comp map-of-factors raw-factors))

(defn mult-factors [mp1 mp2]
  (merge-with + mp1 mp2))

(defn unwrap-factors [m]
  (mapcat #(let [b (first %)
                 p (second %)]
             (map (fn [_] b) (range p))) m))

(defn full-join [f r1 r2]
  (letfn [(join [e]
            (map #(f e %) r2))]
    (map join r1)))
