(ns app.prob67
  (:require [app.prob18 :as p18]
            [clojure.edn :as edn]
            [clojure.string :as s]))

(defn fix0n [s]
  (s/replace s #"\s0", " "))

(let [v (edn/read-string (str "[" (fix0n (slurp "resources/p067_triangle.txt")) "]"))
      m (p18/to-2d v)]
  (p18/solve v))

