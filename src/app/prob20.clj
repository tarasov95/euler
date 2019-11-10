(ns app.prob20
  (:require [app.utils :as u]) )
;; https://projecteuler.net/problem=20

(defn red10 [n]
  (if (= 0 (mod n 10))
    (quot n 10)
    n))

(defn fac
  ([n] (fac n 1))
  ([n r]
   (if (= n 1)
     r
     (recur (dec n) (red10 (* r n))))))

(defn dig10 [n]
  (if (= n 0)
    []
    (conj
     (dig10 (quot n 10))
     (mod n 10))))

(defn factor-fac [n]
  (let [pf (map u/prime-factors (range 2 (inc n)))]
   (into (sorted-map) (reduce #(u/mult-factors %1 %2) pf))))

(defn factor-red10 [m]
  (let [p2 (m 2)
        p5 (m 5)]
    (if (> p2 p5)
      (dissoc (assoc m 2 (- p2 p5)) 5)
      (dissoc (assoc m 5 (- p5 p2)) 2))))

(defn mod-prod [b n1 n2]
  (mod (* (mod n1 b) (mod n2 b)) b))

(defn unwrap-factors [m]
  (mapcat #(let [b (first %)
                 p (second %)]
             (map (fn [_] b) (range p))) m))

;; (letfn [(test-mod-prod [n1 n2]
;;           [(* n1 n2) (mod-prod n1 n2)])]
;;   (let [f10  (factor-fac 10)
;;         f100 (factor-fac 100)]
;;     {:10  (factor-red10 f10)
;;      :e0 (unwrap-factors(factor-red10 f10))
;;      :e1 (reduce (partial mod-prod 10) (unwrap-factors(factor-red10 f10)))
;;      :e2 (u/prime-factors 88)
;;      :e3 (reduce (partial mod-prod 100) (unwrap-factors {2 6, 3 4, 7 1}))
;;      :d   (dig10 (fac 10))}))

;; (defn prod-intl [v n]
;;   (let [f (first v)
;;         s (+ (or f 0) n)]
;;     (println f s v n)
;;     (if (< s 10)
;;       (conj (rest v) s)
;;       (conj
;;        (prod-intl (rest v) (quot s 10))
;;        (mod s 10)))))

;; (prod-intl [1 2] 34)

(defn prod
  ([v n] (prod v n 0))
  ([v n c]
   (let [f (first v)
         r (rest v)
         f0 (if (nil? f) 0 f)
         s (+ (* f0 n) c)]
     (cond (and (empty? r) (<  s 10)) (conj r s)
           :else (conj
                  (prod r n (quot s 10))
                  (mod s 10))))))

(defn vec2dec [v]
  (let [f (first v)
        r (rest v)]
    (if (nil? f)
      0
      (+ f (* 10 (vec2dec r))))))

;; (letfn [(test-prod [v n]
;;           {:vec2dec (vec2dec v)
;;            :prod (reverse (prod v n))
;;            "*" (* n (vec2dec v))})]
;;   [
;;   (test-prod [1 2] 321)
;;   (test-prod [1] 324)
;;   (test-prod [9 4] 3)
;;   (test-prod [0 2 1] 6)
;;   ])

(defn fac-vec [n]
  (if (= 1 n)
    (list 1)
    (prod (fac-vec (dec n)) n)))

[
 (apply + (fac-vec 10))
 (apply + (fac-vec 100))
 ]
