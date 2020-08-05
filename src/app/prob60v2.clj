(ns app.prob60v2
  (:require [lib.prime-data :as data]
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
;; def isPrime(n, k):
;; if n < 2: return False
;; if n < 4: return True
;; if n % 2 == 0: return False    # speedup

;; # now n is odd > 3
;; s = 0
;; d = n-1
;; while d % 2 == 0:
;; s += 1
;; d //= 2
;; # n = 2^s * d where d is odd

;; for i in range(k):
;; a = random.randrange(2, n-1)    # 2 <= a <= n-2
;; x = (a**d) % n
;; if x == 1: continue
;; for j in range(s):
;; if x == n-1: break
;; x = (x * x) % n
;; else:
;; return False
;; return True

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

(defn ^:private mr-a [t]
  ;; (rand-mr 2 (dec n))
  (->> (list 2 3 5 7 11 13 17 19 23 29 31 37 41)
       (take-while #(<= % t))))

(defn prime-mr? [n]
  (cond
    (< n 2) false
    (< n 4) true
    (= 0 (mod n 2)) false
    :else (let [[d s] (prime-mr-ds n)]
            (loop [ra (mr-a (dec 1))]
              (if (empty? ra)
                true
                (let [a (first ra)
                      x (numb/pow-mod n a d)]
                  (if (or (= x 1) (= x (dec n)))
                    (recur (rest ra))
                    (if (= nil (loop-j-mr x n s))
                      (recur (rest ra))
                      false))))))))

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


;; (->> data/prime-seed
;;      (take 100)
;;      (map prime-mr?))
(time (count (filter not (map prime-mr? data/prime-seed ))))
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


