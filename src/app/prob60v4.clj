(ns app.prob60v4
  (:require [lib.prime :as prime]
            [lib.prime-data :as data]
            [lib.seq :as sq]
            [clojure.set :as st]
            [clojure.test :as t]
            [lib.numb :as numb]))

(defn cat-left [x y]
  (read-string (str x y)))

(t/deftest cat-left-test
  (t/is (= 737 (cat-left 7 37)))
  (t/is (= 6737 (cat-left 673 7))))

(defn cat-right [x y]
  (cat-left y x))

(defmacro lsqrt [n]
  `(long (Math/sqrt ~n)))

(defn ^:private zero-remain? [x y]
  (= 0 (mod x y)))

(defn prime-seed? [seed n]
  (cond
    (< n 2) false
    (< n 4) true
    (even? n) false
    :else (let [q (lsqrt n)]
            (nil? (transduce (comp
                         (take-while (partial >= q))
                         (filter (partial zero-remain? n))
                         (take 1))
                        sq/transduce-and seed)))))

(def check-prime prime/prime-mr?)
;; (def check-prime prime/prime-div?)
;; (defn check-prime [n] (prime-seed? data/prime-seed n))

(defn in-family? [rg p]
  (not (transduce
        (comp
         (filter #(not
                   (and (check-prime (cat-right % p))
                        (check-prime (cat-left % p)))))
         (take 1))
        sq/transduce-and rg)))

(defn kissing? [p1 p2]
  (and (check-prime (cat-right p1 p2))
       (check-prime (cat-left p1 p2))))


(defn list-all-kissing-pairs [N]
  (let [pr (take N data/prime-seed)]
   (for [p1 pr
         p2 pr
         :when (and (> p2 p1)
                    (kissing? p1 p2))]
     #{p1 p2})))

(defn graph [rk]
  (reduce #(assoc %1 (str (reduce min %2) "," (reduce max %2)) true) {} rk))

(defn has-edge? [g v1 v2]
  (g (str (min v1 v2) "," (max v1 v2))))

(defn clique? [K g p1 p2]
  (let [i (st/intersection p1 p2)]
   (and (= K (count i))
        (has-edge? g
                   (first (st/difference p1 i))
                   (first (st/difference p2 i))))))

(defn clique-step [rk g K]
  (into [] (distinct)
        (let [cnt (count rk)]
          (letfn [(next-w [w v1 v2]
                    (if (clique? K g v1 v2)
                      (conj w (into (sorted-set) (concat v1 v2)))
                      w))
                  (loop-j [i]
                    (loop [j (inc i)
                           w []]
                      (if (= j cnt)
                        w
                        (recur (inc j) (next-w w (rk i) (rk j))))))]
            (loop [i 0
                   z []]
              (if (= i cnt)
                z
                (recur (inc i) (into [] (concat z (loop-j i))))))))))

(defn clique
  ([N K] (let [z (into [] (list-all-kissing-pairs N))
               g (graph z)]
           (clique (clique-step z g 1) g 1 K)))
  ([z g ix K]
   (if (or (= ix (- K 2)) (empty? z))
     z
     (recur (clique-step z g (inc ix)) g (inc ix) K))))

(time (println (take 10 (clique 1150 5))))
