(ns app.prob53
  (:require
   [lib.prime :as prime]
   [lib.seq :as sq]
   [lib.numb :as numb]))

;; https://projecteuler.net/problem=53
;; How many, not necessarily distinct, values of C(n,r) for 1 <= n <= 100, are greater than one-million?

(def fact-tail
  (memoize
   (fn [s e]
     (cond
       (> s e) nil
       (= s e) 1
       (= s (dec e)) e
       :else (* e (fact-tail s (dec e)))))))

(defn C
  "C => n! / r!(n-r)!"
  [n r]
  (/ (fact-tail (max r (- n r)) n)
     (numb/fact (min r (- n r)))))

(defn for-C [S N]
  (->> (for [n (range S (inc N))
             r (range S (inc N))
             :when (<= r n)]
         [n r])))

(defn solve-bigint []
  (count (->> (for-C 1N 100N)
              (map #(conj % (C (first %) (second %))))
              (filter #(> (last %) 1000000)))))

(defn test-fact-tail-div-fact
  ([s e div] (test-fact-tail-div-fact 1.0 s e (max 1 div)))
  ([z s e div]
   ;; (println z s e div)
   (cond
     (> s e) nil
     (and (= 1 div) (> z 1000000)) true
     (= s e) (> (/ z div) 1000000)
     :else (recur (* z (/ e div)) s (dec e) (max 1 (dec div))))))

(defn solve-test []
  (count (->> (for-C 1 100)
              (filter #(let [[n r] %
                             df (- n r)
                             mi (min r df)
                             ma (max r df)]
                         (test-fact-tail-div-fact ma n mi))))))

(time (println "solve-bigint" (solve-bigint)))
(time (println "solve-test" (solve-test)))
