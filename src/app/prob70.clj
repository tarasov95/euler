(ns app.prob70
  (:require [clojure.test :as t]
            [lib.prime :as prime]
            [taoensso.tufte :as tufte :refer (defnp p profiled profile)]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=70

;; n/f ~> n/(n-1) when n is a prime
;; p*(1/(1-1/p))
;; min(n/f) ?
;; n/f ~> 1/P(1-1/p) for each p dividing n
;; ~> have to minimize 1/P(1-1/p) ~> max P(1-1/p)
;; ~> n would be the one with the smallest number of prime factors as large as possible

(defn permut? [n1 n2]
  (let [s1 (seq (str n1))
        s2 (seq (str n2))]
    (= (group-by identity s1)
       (group-by identity s2))))

(t/deftest permut?-test
  (t/is  (not (permut? 9270016N 9276109)) )
  (t/is (not (permut? 9784303 9778048)))
  (t/is (not (permut? 9778048 9784303)))
  (t/is (not (permut? 313 31)))
  (t/is (not (permut? 133 312)))
  (t/is (permut? 133 313))
  (t/is (permut? 123 312)))

(defn P [n]
  (->> (prime/prime-fact n)
      (map :fac)
      (map #(- 1 (/ 1 %)))
      (reduce *)))

(defn phi [n]
  (* n (P n)))

(def ^:dynamic *N* 1000000)

(defn append-fact [n r]
  (p :append-fact-body
     {:n (* n (:n r)) ;;multiplication of all factors
      :f (conj (:f r) n) ;;set of distinct factors
      :Pn (if ((:f r) n) ;; Pn ~> P(1-1/p) of all distinct factors
            (:Pn r)
            (* (- 1 (/ 1 n)) (:Pn r)))}))

(defn new-rec [n]
  {:n n :f #{n} :Pn (- 1 (/ 1 n))})

(defn rec-phi [r]
  (* (:n r) (:Pn r)))

(t/deftest rec-phi-test
  (t/is (= (phi 6) (rec-phi (append-fact 3 (new-rec 2)))))) ;;(phi 6)

(defn solution? [r]
  (p :solution?-body
     (permut? (:n r) (rec-phi r))))

;; (defn add-next-fact [z n]
;;   (p :add-next-fact-body
;;      (->> z
;;           (filter #(< (* n (:n %)) *N*))
;;           (map #(append-fact n %)))))

(defn add-next-fact [a n]
  (p :add-next-fact-body
     (loop [z (list)
            rg a]
       (let [r (first rg)]
         (cond
          (empty? rg) z
          (< (* n (:n r)) *N*) (recur (conj z (append-fact n r)) (rest rg))
          :else (recur z (rest rg)))))))

(defn add-next-fact-loop [z n]
  (let [rec (new-rec n)]
    (if (solution? rec)
      [true [rec]]
      (let [rgn (add-next-fact z n)
            Z (filter solution? rgn)]
        (cond
          (p :empty?-z (empty? z)) [false [rec]]
          (p :empty?-rgn (empty? rgn)) [false (p :conj-rec (conj z rec))]
          (p :not-empty-Z (not-empty Z)) [true Z]
          :else [false (p :conj-else (conj (into (list) (concat z rgn)) rec))])))))

(defn find-solution
  ([] (let [rrp (p :prime-seed (into (list) (prime/primes-below (inc (/ *N* 2)))))] ;;into list reverses the seq
        (find-solution (list) rrp)))
  ([z rrp]
   (if (empty? rrp)
     nil
     (let [n (first rrp)
           y (add-next-fact-loop z n)]
       (if (first y)
         [(second y) (count z) (take 10 z)]
         (recur (second y)
                (rest rrp)))))))


(defn sample [N]
  (->> (range 2 N)
      (map #(vector % (phi %)))
      (map #(conj % (/ (float (first %)) (second %))))
      (filter #(permut? (first %) (second %)))
      (sort-by last)
      (first)))


(tufte/add-basic-println-handler! {})
(profile
 {}
 (binding [*N* 100000]
   ;; (time (println "sample" (sample *N*)))
   (time (println "find-solution" (find-solution)))))

