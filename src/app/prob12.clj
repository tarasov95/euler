(ns app.prob12
  (:require [app.prob3 :as p3]) )

;; https://projecteuler.net/problem=12

(defn trin [ix]
  (/ (* ix (+ 1 ix)) 2))

(defn raw-factors [n]
  (p3/list-prime-factors p3/list-of-dividers n))

;; (defn readable-factors [rg]
;;   (map (fn [e] {:prime (last e) :power (second e)}) rg))

(defn map-of-factors [rg]
  (reduce (fn [r e] (assoc r (last e) (second e))) {} rg))

;; (defn count-factor-pow [rg]
;;   (apply * (map (fn [e] (+ 1 (:power e))) rg)))

(defn count-factor-pow [mp]
  (apply * (map (fn [e] (+ 1 (second e))) mp)))

(defn mult-factors [mp1 mp2]
  (merge-with + mp1 mp2))

(def factors (comp map-of-factors raw-factors))
(def count-dividers (comp count-factor-pow factors))

(defn div2 [mp]
  (let [v (dec (get mp 2))]
    (if (= 0 v)
      (dissoc mp 2)
      (assoc mp 2 v))))

(defn seek [ix fac]
  (let [ix1 (+ 1 ix)
        nex (factors ix1)
        mul (div2 (mult-factors nex fac))
        cnt (count-factor-pow mul)]
    (println {:ix ix :n (trin ix) :mul mul :cnt cnt})
    (when (<= cnt 500)
      (recur ix1 nex))))

(seek 1 (assoc {} 1 1))
;; (lazy-seq)
;; (let [f1 (factors 10)
;;       f2 (factors 14)]
;;   {:f1 f1
;;    :f2 f2
;;    :x (count-factor-pow f2)
;;    :m (mult-factors f1 f2)})
;; (map-of-factors (raw-factors 28))
;; (let [rg (raw-factors 28)
;;       rd (readable-factors rg)]
;;   {:rg             rg
;;    :readable       rd
;;    :count-dividers (count-dividers rd)})

;; {:v1 (count-dividers 28)
;;  :v2 (count-dividers 29)
;;  :rg1 (factors 28)
;;  :rg2 (factors 29)}
;; (map (fn [n] {:count (count-dividers n)
;;               :n n
;;               :factors (factors n)})
;;      (map trin (range 1 10)))
;; (apply max (map count-dividers (map trin (range 1 6000))))
