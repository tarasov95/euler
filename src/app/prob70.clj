(ns app.prob70
  (:require [clojure.test :as t]
            [lib.prime :as prime]
            [clojure.core.reducers :as r]
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
    (and
     (= (count s1) (count s2))
     (= (group-by identity s1)
        (group-by identity s2)))))

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
       (r/map :fac)
       (r/map #(- 1 (/ 1 %)))
       (r/reduce *)))

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
     (loop [z a
            rg a
            Z nil]
       (let [r (first rg)]
         (cond
           (not-empty Z) [z Z]
           (empty? rg) [z Z]
           (< (* n (:n r)) *N*) (let [y (append-fact n r)]
                                  (recur (conj z y)
                                         (rest rg)
                                         (if (solution? y) (conj (or Z (list)) y) Z)))
           :else (recur z (rest rg) Z))))))

(defn add-next-fact-loop [z n]
  (let [rec (new-rec n)]
    (if (solution? rec)
      [true [rec]]
      (let [rg0 (add-next-fact z n)
            rgn (first rg0)
            Z (second rg0)]
        (cond
          (p :empty?-z (empty? z)) [false [rec]]
          (p :empty?-rgn (empty? rgn)) [false (conj z rec)]
          (p :not-empty-Z (not-empty Z)) [true Z]
          :else [false (conj z rec)])))))

(defn find-solution
  ([] (let [rrp (p :prime-seed (into (list) (prime/primes-below (inc (/ *N* 10)))))] ;;into list reverses the seq
        (find-solution (list) rrp)))
  ([z rrp]
   (if (empty? rrp)
     nil
     (let [n (first rrp)
           y (add-next-fact-loop z n)]
       (if (first y)
         [(second y) (count z)]
         (recur (second y)
                (rest rrp)))))))


(defn brute-search [N]
  (->> (range 2 N)
       (map #(vector % (phi %)))
       (map #(conj % (/ (float (first %)) (second %))))
       (filter #(permut? (first %) (second %)))
       (sort-by last)
       (first)))


(tufte/add-basic-println-handler! {})
(comment (profile
  {}
  (binding [*N* 100000]
    ;; (time (println "sample" (sample *N*)))
    (time (println "find-solution" (find-solution))))))


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

(defn find-in-range [S E]
  (loop [rg (range S E)
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

(let [N (long 1e6)]
 (time (println "find-in-range" (find-in-range 2 N)))
 (time (println "brute-search" (brute-search N))))

