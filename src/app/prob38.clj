(ns app.prob38
  (:require [lib.numb :as numb]))

;; https://projecteuler.net/problem=38
;; By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 the concatenated product of 192 and (1,2,3)
;; The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).
;; What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with (1,2, ... , n) where n > 1?

;;result => 932718654
;; when number = 1 ~> n is (range 1 10), any other number the range is narrower
;; when number = 9 the concatenation is 918273645 ~> have to look only for concatenations greater than that
;; when n = 1 ~> the number is in the 1st serveral digits of the concatenation
;; all digits of the driving number must be different

(defn lgpw10 [y]
  (numb/pow-int 10 (inc (numb/log-int 10 y))))

(defn try-number [num]
  (loop [n 1
         z 0]
    (let [lg10 (numb/log-int 10 (max z 1))
          y (* num n)]
      (cond (> n 9) nil
            (= lg10 8) z
            (> lg10 8) nil
            :else (recur
                   (inc n)
                   (+ y (* z (lgpw10 y))))))))

;; the number must start with 9 and be itself pandigital
;; TODO: count digits in a number using a bit mask of length 9
(defn solve []
  (->> (range 1 876)
       (map #(+ % (* 9 (lgpw10 %))))
       (map try-number)
       (filter #(not= nil %))
       (filter #(let [d (into #{} (str %))]
                  (and (= nil (d \0))
                       (= 9 (count d)))))
       (reduce max)))

(time (solve))
