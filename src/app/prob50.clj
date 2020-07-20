(ns app.prob50
  (:require
   [lib.prime :as prime]
   [lib.numb :as numb]))

;; https://projecteuler.net/problem=50
;; The prime 41, can be written as the sum of six consecutive primes:
;; 41 = 2 + 3 + 5 + 7 + 11 + 13
;; This is the longest sum of consecutive primes that adds to a prime below one-hundred.
;; The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.
;; Which prime, below one-million, can be written as the sum of the most consecutive primes?

(defn len [s]
  (-> s second count))

(defn max-len-sum [s1 s2]
  (if (> (len s1) (len s2))
    s1
    s2))

(defn solve [N]
  (let [*pseq* (prime/primes-below N)
        *pset* (into #{} *pseq*)]
    (loop [rg   *pseq*
           sums [] ;;[sum [addends]]
           z    [0 []]]
      (if (empty? rg)
        z
        (let [p (first rg)
              next (->>
                    sums
                    (map #(vector
                           (+ p (first %))
                           (conj (second %) p)))
                    (cons [p [p]])
                    (filter #(<= (first %) N)))]
          (recur
           (rest rg)
           next
           (->> next
                (filter (comp *pset* first))
                (reduce (partial max-key len))
                (max-len-sum z))))))))

(time (first (solve 1000000)))
