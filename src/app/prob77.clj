(ns app.prob77
  (:require [clojure.test :as t]
            [lib.prime :as prime]
            [lib.seq :as sq]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=77

;; It is possible to write ten as the sum of primes in exactly five different ways:
;; What is the first value which can be written as the sum of primes in over five thousand different ways?

;; (def count-sums
;;   (fn [N c]
;;     (if (or (= 1 N) (= 1 c))
;;       1
;;       (let [cn (dec c)]
;;         (+ (count-sums N cn)
;;            (->> (range c (inc N) c)
;;                 (map #(count-sums (- N %) cn))
;;                 (reduce +)))))))

(defn change-one [n p]
  (if (and (not= 0 n) (zero? (mod n p)))
    (do
      ;; (println "change-one" n p)
      (repeat (quot n p) p))
    nil))

(t/deftest change-one-test
  (t/is (= [2 2 2 2] (change-one 8 2)))
  (t/is (= nil (change-one 8 3))))

(defn prime-ways
  ([n] (prime-ways [] n (prime/primes-below (- n 2))))
  ([z n rp]
   ;; (println z n rp)
   (cond
     (< n 2) z
     (empty? (rest rp)) (sq/if-conj z (change-one n (first rp)))
     :else (let [fp (first rp)
                 rpn (rest rp)]
             (into []
                   (->> (range 0 (quot n fp))
                        (mapcat #(sq/each concat
                                          (repeat % fp)
                                          (prime-ways [] (- n (* % fp)) rpn)))
                        (concat (prime-ways [] n [fp]))
                        (concat z)))))))

(t/deftest prime-ways-test
  (t/is (= 5 (count (prime-ways 10)))))

(defn pways-count
  ([n] (pways-count n (prime/primes-below (- n 2))))
  ([n rp]
   (let [fp (first rp)
         rpn (rest rp)]
     (cond
       (= 0 (quot n fp)) 0
       (>= fp n) (if (zero? (mod n fp)) 1 0)
       (empty? rpn) (if (zero? (mod n fp)) 1 0)
       :else (+ (pways-count n [fp])
                (->> (range 0 (quot n fp))
                     (map #(pways-count (- n (* % fp)) rpn))
                     (reduce +)))))))

(t/deftest pways-count-test
  (t/is (= 5 (pways-count 10))))

(defn pways-rev
  ([n] (pways-rev n (reverse (prime/primes-below 20))))
  ([n rp]
   (let [fp (first rp)
         rpn (rest rp)]
     (cond
       (< n 2) 0
       ;; (= fp n) (if (zero? (mod n fp)) 1 0)
       (empty? rpn) (if (zero? (mod n fp)) 1 0)
       :else (+ (pways-rev n [fp])
                (->> (range 0 (inc (quot n fp)))
                     (map #(pways-rev (- n (* % fp)) rpn))
                     (reduce +)))))))

(defn prime-below [n]
  (last (take-while (partial > n)
                    prime/*prime-feed*)))

(t/deftest prime-below-test
  (t/is (= 5 (prime-below 7)))
  (t/is (= 7 (prime-below 10))))

(defn pways2
  ([n] (pways2 n (prime-below n)))
  ([n p]
   (let [fp p
         rpn (prime-below p)]
     (cond
       (< n 2) 0
       (= fp 2) (if (zero? (mod n fp)) 1 0)
       :else (+ (if (zero? (mod n fp)) 1 0)
                (->> (range 0 (inc (quot n fp)))
                     (map #(pways2 (- n (* % fp)) rpn))
                     (reduce +)))))))

(t/deftest solve-recur
  (t/is
   (= [71 5006] (->> (drop 10 (range))
                     (map #(vector % (pways2 %)))
                     (filter #(>= (second %) 5000))
                     (first)))))

(defn init-dps [n]
  (->> (range 2 (inc n) 2)
       (reduce (fn [z e] (assoc z [e 2]
                                (if (even? e) 1 0))) {})))

(t/deftest init-dps-test
  (t/is (= {[2 2] 1, [4 2] 1}
           (init-dps 5))))

(defn ^:private pways-next [n f p s]
  (+ (if (zero? (mod n p)) 1 0)
     (->> (range n 0 (- p))
          (map #(or (s [% f]) 0))
          (reduce +))))

(defn pways-dp
  ([n] (pways-dp (init-dps n) (prime/primes-below n) n))
  ([s rp n]
   (let [f (first rp)
         p (second rp)]
     (if (or (nil? p) (>= p n))
       (s [n f])
       (recur (->> (range 1 (inc n))
                   (map #(vector [% p] (pways-next % f p s)))
                   (into {}))
              (rest rp)
              n)))))

[(pways-dp 76)
 (pways2 76)]
