(ns prob5
  (:require [prob3 :as p3]))

(def N1 2520)
(def N2 232792560)

(defn pf [n]
  (map (comp reverse (partial into []) rest) (p3/list-prime-factors n)))

(defn max-ppow "fUNCTION rESULT eLEMENT" [f r e]
  (let [[p w] e]
    (assoc r p (f w (or (get r p) 0)))))

(defn map-pow [mf]
  (map #(let [[p w] %] (long (Math/pow p w))) mf))

(defn prob5 "sTART eND" [s e]
  (let [rg (range (inc s) (inc e))
        rf (mapcat pf rg)
        mf (reduce (partial max-ppow max) {} rf)]
    (reduce * (map-pow mf))))

{:test (prob5 1 10)
 :prob (prob5 1 20)}

;; (let [rg (range 2 21)
;;       rf (mapcat pf rg)
;;       mf (reduce (partial max-ppow max) {} rf)]
;;   {:mf mf
;;    :mp (reduce * (map-pow mf))
;;    :sf (reduce (partial max-ppow +) {} rf)
;;    :rf rf})

;; (map {} (1 2) (:a :b))
;; (let [rg (range 2 21)
;;       n (reduce * (into #{} (mapcat pf rg)))
;;       t (* 3 8 n)
;;       rgMod (map (partial mod t) rg)]
;;   {:t t
;;    :mod rgMod
;;    :rge (map vector rgMod rg)})
