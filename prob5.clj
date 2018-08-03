(ns prob5
  (:require [prob3 :as p3]))

(def N1 2520)
(def N2 232792560)

(defn pf [n]
  (map last (p3/list-prime-factors n)))

;; {"primes of N1" (pf N1)
;;  "primes of the range" (into #{} (mapcat pf (range 2 11)))}

;; {"primes of N2" (pf N2)
;;  "primes of the range" (into #{} (mapcat pf (range 2 21)))}

;; (p3/list-prime-factors 19)
;; (mod N2 19)
;; 1) start with product of all primes
;; 2) get mod of the numbers of the test range
;; 3) for non-zero mod output add primes of the corresponding test number ot the product
;; 4) repeat from p.2
(let [rg (range 2 21)
      n (reduce * (into #{} (mapcat pf rg)))
      t (* 3 8 n)
      rgMod (map (partial mod t) rg)]
  {:t t
   :mod rgMod
   :rge (map vector rgMod rg)})
