(ns app.prob60v2
  (:require [lib.prime-data :as data]
            [lib.prime :as prime]
            [clojure.core.reducers :as r]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=60
;; The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating them in any order the result will always be prime. For example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these four primes, 792, represents the lowest sum for a set of four primes with this property.
;; Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.

;; https://projecteuler.net/overview=007
;; 1 is not a prime.All primes except 2 are odd.All primes greater than 3 can be written in the form  6k+/-1.Any number n can have only one primefactor greater than sqrt(n).The consequence for primality testing of a number n is: if we cannot find a numberf less than or equal sqrt(n) that divides n then n is prime: the only primefactor of n is n itself

(def pw10 (memoize (partial numb/pow-int 10)))
(def lg10 (memoize (partial numb/log-int 10)))

(defn def-prime-seed [seed]
  (let [lp (last seed)]
    (with-meta
      (apply vector (map long seed))
      ;; (apply list (map long seed))
      {:start (if (= 0 (mod (dec lp) 6))
                (- lp 2)
                (+ lp 4))
       :count (count seed)})))

;; (def test-prime-seed (def-prime-seed [2 3 5 7 11]))
(def test-prime-seed (def-prime-seed (subvec data/prime-seed 0 500)))

(defn prime-div?
  ;;https://projecteuler.net/overview=007
  ([n] (prime-div? 5 n))
  ([start n]
   (cond
     (= n 1) false
     (< n 4) true
     (= 0 (mod n 2)) false
     (< n 9) true
     (= 0 (mod n 3)) false
     :else (let [q (Math/sqrt n)]
             (loop [f start]
               (cond (> f q) true
                     (= 0 (mod n f)) false
                     (= 0 (mod n (+ 2 f))) false
                     :else (recur (+ 6 f))))))))

(defn lsqrt [n]
  (long (Math/sqrt n)))

(comment (defn prime-seed? [seed q cnt n]
   (loop [ix 0]
     (let [p (seed ix)
           next-ix (inc ix)]
       (cond (= next-ix cnt) nil
             (> p q) true
             (= 0 (mod n p)) false
             :else (recur next-ix))))))

(defn ^:private and-rez
  ([] nil)
  ([b] b)
  ([b1 b2]
   (cond (and (= nil b1) (= nil b2)) true
         (= nil b1) b2
         (= nil b2) b1
         :else (and b1 b2))))

(defn prime-seed? [seed q cnt n]
  (transduce (comp
              (take-while (partial >= q))
              (filter #(= 0 (mod n %)))
              (take 1))
             and-rez seed))

(comment (defn prime-seed? [seed q cnt n]
   (->> seed
        (r/take-while (partial >= q))
        (r/filter #(= 0 (mod n %)))
        (r/take 1)
        (r/fold and-rez))))

(defn prime? [seed n]
  (let [q (lsqrt n)
        sm (meta seed)
        z (prime-seed? seed q (:count sm) n)]
    (if (= nil z)
      (loop [f (:start sm)]
        (cond (> f q) true
              (= 0 (mod n f)) false
              (= 0 (mod n (+ 2 f))) false
              :else (recur (+ 6 f))))
      z)))

(defn rand-mr [x1 x2]
  (+ x1 (rand-int (- x2 x1))))

(defn ^:private prime-mr-ds [n]
  (loop [s 0
         d (dec n)]
    (if (= 0 (mod d 2))
      (recur (inc s) (quot d 2))
      [d s])))

(defn ^:private loop-j-mr [x n s]
  (loop [j (dec s)
         y (mod (* x x) n)]
    (cond (= j 0) false
          (= y 1) false
          (= y (dec n)) nil
          :else (recur (dec j) (mod (* y y) n)))))

(def n2047 [2])
(def n1373653 [2 3])
(def n9090191 [31 73])
(def n25326001 [2 3 5])
(def n3215031751 [2 3 5 7])
(def n1122004669633 [2 13 23 1662803])
(def n2152302898747 [2 3 5 7 11])
(def n3474749660383 [2 3 5 7 11 13])
(def n341550071728321 [2 3 5 7 11 13 17])
(def n3825123056546413051 [2 3 5 7 11 13 17 19 23])

(defn ^:private mr-a [n]
  ;; (rand-mr 2 (dec n))
  (cond
    (< n 2047) n2047
    (< n 1373653) n1373653
    (< n 9090191) n9090191
    (< n 25326001) n25326001
    (< n 3215031751) n3215031751
    (< n 1122004669633) n1122004669633
    (< n 2152302898747) n2152302898747
    (< n 3474749660383) n3474749660383
    (< n 341550071728321) n341550071728321
    (< n 3825123056546413051) n3825123056546413051
    :else (throw (new Exception "n is too large."))))

(defn prime-mr? [n]
  (cond
    (< n 2) false
    (< n 4) true
    (= 0 (mod n 2)) false
    :else (let [[d s] (prime-mr-ds n)
                ra (mr-a n)]
            (loop [ix (dec (count ra))]
              (if (< ix 0)
                true
                (let [a (ra ix)
                      x (numb/pow-mod n a d)]
                  (if (or (= x 1) (= x (dec n)))
                    (recur (dec ix))
                    (if (= nil (loop-j-mr x n s))
                      (recur (dec ix))
                      false))))))))

;; (mr-a 7)
;; (prime-mr-ds 7)
;; (pow-mod 7 2 3 )
;; (prime-mr? 7)
;; (->> data/prime-seed
;;      (take 100)
;;      (map prime-mr?))
(let [rg (range 2 10000000)]
  ;; (time (println "is-prime?" (count (filter not (map prime/is-prime? rg)))))
  (time (println "prime-mr?" (count (filter not (map prime-mr? rg))))))
;; [(prime-mr? 7)
;;  (prime-mr? 11)
;;  (prime-mr? 15)]

;; (take 10 test-prime-seed)
;; (meta test-prime-seed)
;; (let [n 8
;;       seed test-prime-seed]
;;   (prime-seed? seed (lsqrt n) (count seed) n))
;; (time (count (filter not (map (partial prime? test-prime-seed) data/prime-seed ))))
;; (time (count (filter not (map prime-div? data/prime-seed ))))


