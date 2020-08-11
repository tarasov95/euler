(ns app.prob64
  (:require [clojure.test :as t]
            [lib.polygonal :as pn]
            [lib.numb :as numb]))


;;https://en.wikipedia.org/wiki/Periodic_continued_fraction
(defn next-a [p]
  (let [[a m d S a0 len ix] p
        mn (- (* d a) m)
        dn (/ (- S (* mn mn)) d)
        an (long (Math/floor (/ (+ a0 mn) dn)))
        ixn (cond (< ix 0) (inc (- ix))
                  (= (* 2 a0) an) (- ix)
                  :else ix)
        lenn (if (or (= -1 ixn) (= 2 ixn)) (inc len) len)]
    [an mn dn S a0 lenn ixn]))

(defn intl-seq [S fn-select]
  (into []
        (comp (take-while (fn [r] (<= (last r) 2)))
              fn-select)
        (iterate next-a [(numb/lsqrt S) 0 1 S (numb/lsqrt S) 0 1])))

(defn a-seq [S]
  (intl-seq S (map first)))

(t/deftest a-seq-test
  (t/is [1 2 2 2] (a-seq 2)))

(defn period [S]
  (->> (intl-seq S (map (comp last butlast))) ;;take len, which is 2nd from the end
       (last))) ;;of the very last item

(t/deftest period-test
  (t/is 5 (period 13)))

(defn list-fractions [N fn-select]
  (sequence
   (comp
    (filter #(-> %  Math/sqrt numb/natural? not))
    fn-select)
   (range 2 (inc N))))

(t/deftest list-fractions-test
  (t/is [1 2 2 2] (first (list-fractions 2 (map a-seq)))))

(t/deftest solve
  (t/is 1322
        (time
         (count
          (list-fractions 10000
                          (comp (map period)
                                (filter odd?)))))))


