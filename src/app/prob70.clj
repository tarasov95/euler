(ns app.prob70
  (:require [clojure.test :as t]
            [lib.prime :as prime]
            [clojure.core.reducers :as r]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=70
;; https://projecteuler.net/best_posts=070

;; n/f ~> n/(n-1) when n is a prime
;; p*(1/(1-1/p))
;; min(n/f) ?
;; n/f ~> 1/P(1-1/p) for each p dividing n
;; ~> have to minimize 1/P(1-1/p) ~> max P(1-1/p)
;; ~> n would be the one with the smallest number of prime factors as large as possible

(defn permut? [n1 n2]
  (let [s1 (seq (str n1))
        s2 (seq (str n2))]
    (and
     (= (count s1) (count s2))
     (= (group-by identity s1)
        (group-by identity s2)))))

(t/deftest permut?-test
  (t/is  (not (permut? 9270016N 9276109)))
  (t/is (not (permut? 9784303 9778048)))
  (t/is (not (permut? 9778048 9784303)))
  (t/is (not (permut? 313 31)))
  (t/is (not (permut? 133 312)))
  (t/is (permut? 133 313))
  (t/is (permut? 123 312)))

(defn P [n]
  (->> (prime/prime-fact n)
       (r/map :fac)
       (r/map #(- 1 (/ 1 %)))
       (r/reduce *)))

(defn phi [n]
  (* n (P n)))

(t/deftest rec-phi-test
  (t/is (= (phi 6) (rec-phi (append-fact 3 (new-rec 2)))))) ;;(phi 6)

(defn fac2pow
  [n p]
  (loop [pow 0 ;;power of the factor
         ppow 1 ;;factor to the power
         cur n]
    (cond
      (> pow 1) nil
      (= 0 (mod cur p)) (recur (inc pow) (* ppow p) (quot cur p))
      :else ppow)))

(defn pfact
  ([n] (pfact 1 1 2 n))
  ([z y fact-guess n]
   ;; (println z fact-guess n)
   (cond
     (< n 1) y
     (> (* fact-guess fact-guess) n) (* y (dec n)) ;;it's a prime
     :else
     (let [next-guess (if (< fact-guess 3) 3 (+ fact-guess 2))
           fac (fac2pow n fact-guess)]
       (if (nil? fac)
         nil
         (if (> fac 1)
           (recur (* z fac)
                  (* y (dec fac))
                  next-guess
                  (quot n fac))
           (recur z y next-guess n)))))))

(t/deftest pfact-test
  (t/is (= 7020736 (pfact 7026037))))

(defn find-in-range [src-range]
  (loop [rg src-range
         mi (Long/MAX_VALUE)
         ni nil]
    (if (empty? rg)
      [ni mi]
      (let [n (first rg)
            p (pfact n)]
        (if (nil? p)
          (recur (rest rg) mi ni)
          (let [ph (/ n p)]
            (if (and (< ph mi)
                     (permut? n p))
              (recur (rest rg) ph n)
              (recur (rest rg) mi ni))))))))

(defn brute-search-min
  ([] nil)
  ([e] e)
  ([e1 e2]
   (cond
     (nil? e1) e2
     (nil? e2) e1
     (< (last e1) (last e2)) e1
     :else e2)))

(defn brute-search [src-range]
  (->> (vec src-range)
       (r/map #(vector % (pfact %)))
       (r/filter #(not (nil? (second %))))
       (r/filter #(permut? (first %) (second %)))
       (r/map #(conj % (float (/ (first %) (second %)))))
       (r/fold brute-search-min)))

(comment
  (let [N (long 1e6)]
    (time (println "find-in-range" (find-in-range (range 2 N))))
    (time (println "brute-search" (brute-search N)))))

(defn gen-ranges [N cnt]
  (let [N (long N)
        s (/ N cnt)]
    (->>
     (iterate #(vector (max 2 (- (first %) s)) (first %))
              [(- N s) N])
     (take cnt))))

(defn psolve []
  (->> (gen-ranges 1e7 10)
       (map (partial apply range))
       (pmap find-in-range)
      ;; (map (fn [rg] (future (find-in-range rg))))
       (reduce brute-search-min)))

(time (println "psolve" (psolve)))
