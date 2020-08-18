(ns app.prob70
  (:require [clojure.test :as t]
            [lib.prime :as prime]
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

(def N 1000000)

(defn append-fact [n r]
  {:n (* n (:n r)) ;;multiplication of all factors
   :f (conj (:f r) n) ;;set of distinct factors
   :Pn (if ((:f r) n) ;; Pn ~> P(1-1/p) of all distinct factors
         (:Pn r)
         (* (- 1 (/ 1 n)) (:Pn r)))})

(defn new-rec [n]
  {:n n :f #{n} :Pn (- 1 (/ 1 n))})

(defn add-next-fact [z n]
  (->> z
       (filter #(< (* n (:n %)) N))
       (map (partial append-fact n))))

(defn rec-phi [r]
  (* (:n r) (:Pn r)))

(t/deftest rec-phi-test
  (t/is (= (phi 6) (rec-phi (append-fact 3 (new-rec 2)))))) ;;(phi 6)

(defn solution? [r]
  (permut? (:n r) (rec-phi r)))

(defn find-solution
  ([] (let [rrp (reverse (prime/primes-below (inc (numb/lsqrt N))))]
        (find-solution [] [(new-rec (first rrp))] (rest rrp))))
  ([r z rrp]
   (if (empty? rrp)
     r
     (let [n (first rrp)
           y (add-next-fact z n)
           Z (filter solution? y)]
       (if (and false (not-empty Z))
         Z
         (recur (into [] (concat r Z))
          (conj (into [] (concat z y)) (new-rec n))
               (rest rrp)))))))


(comment (->> (find-solution)
      (map #(vector (/ (float (:n %)) (* (:n %) (:Pn %))) (* (:n %) (:Pn %)) %))
      (sort-by first)))

(->> (range 2 N)
     (map #(vector % (phi %)))
     (map #(conj % (/ (first %) (second %))))
     (filter #(permut? (first %) (second %)))
     (sort-by last)
     (take 8))
