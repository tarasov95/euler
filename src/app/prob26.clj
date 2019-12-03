(ns app.prob26
  (:require [app.utils :as u]))

;; https://projecteuler.net/problem=26


;; (defn dec
;;   ([d] (dec d 8 10 []))
;;   ([d p] (dec d p 10 []))
;;   ([d p c z] "d_ivisor p_recizion c_arry re_z_ult"
;;              (if (> (count z) p)
;;                z
;;                (recur d p
;;                       (* 10 (mod c d))
;;                       (conj z (quot c d))))))

(defn dec-seq
  ([d] (dec-seq d 10))
  ([d c]
   "d_ivisor c_arry"
   (when (> c 0)
     (lazy-seq
      (cons (quot c d)
            (dec-seq d (* 10 (mod c d))))))))

(defn find-cycle
  ([q] (find-cycle (rest q) (into [] (take 1 q)) []))
  ([q m z]
   (if (or (empty? q) (empty? m))
     ;; (if (empty? m) z '())
     (if (and (empty? m) (= (first z) (first q)))
       z
       '())
     (if (= (first q) (first m))
       (find-cycle (rest q) (rest m) (conj z (first m)))
       (if (and (> (count m) 2)
                (apply = (conj (take-last 2 m) (first q))))
         (take 1 q)
         (let [v (find-cycle (reverse m))]
           (if (not-empty v)
             (conj (reverse v) -1)
             (find-cycle (rest q) (conj m (first q)) z))))))))

(defn brent-lam
  ([q] (brent-lam (rest q) (first q) 1 1))
  ([q t po2 lam]
   "se_q_uence t_ortoise p_ower_o_f_2 lam_bda"
   (let [h (first q)]
     (cond (= po2 lam) (brent-lam (rest q) h (* po2 2) 1)
           (= t h) lam
           :else (brent-lam (rest q) t po2 (inc lam))))))

(defn solve []
  (let [r (range 2 24)]
    (map
     (fn [e]
       {:d e :r (/ 1.0 e) :c (find-cycle (dec-seq e))}) r)))

(defn floyd-test [t ix]
  (take 4 (filter #(= (:e1 %) (:e2 %))
                  (map
                   (fn [i] {:e1 (nth t i)
                            :e2 (nth t (* 2 i))
                            :i  i
                            :i2 (* 2 i)}) ix))))
(defn po2 [p]
  (bit-shift-left 1 p))

(defn brent-test [t ix]
  (map
   (fn [i] {:e (nth t i)
            :q (take (- (* 2 (inc i)) (inc i)) (nthrest t (inc i)))
            :i  i
            :i2 (* 2 (inc i))}) ix))

(with-redefs [find-cycle (u/defdbg "find-cycle" find-cycle)]
  ;; (take 20 (dec-seq 7))
  (let [t  (into [] (take 140 (dec-seq 23)))
        ix (range 1 (/ (count t) 2))]
    (println "====================")
    (println t)
    (println (map-indexed (fn [i e] [i e]) t))
    ;; (println (floyd-test t ix))
    (println (brent-test t (map #(dec (po2 %)) (range 0 8)))))
  ;; (find-cycle (take 40 (dec-seq 23)))
  ;; (find-cycle '(8 8 5 0))
  ;; (find-cycle (dec-seq 17))
  ;; (find-cycle (take 20 (dec-seq 14)))
  ;; (find-cycle '(7  8 2  1 75  2 4 1 7 0))
  )
;; (solve)
;; (take 40 (dec-seq 22))
;; (0 4 3 4 7 8 2 6 0 8 6 9 5 6 5 2 1 7 3 9 1 3 0 4 3 4 7 8 2 6 0 8 6 9 5 6 5 2 1 7)


;; (with-redefs [brent-lam (u/defdbg "brent-lam" brent-lam)]
;;  (let [t (take 40 (dec-seq 23))]
;;    [t
;;     (brent-lam t)]))

;;x0 -> 1
;;x1 -> 2..4
;; (let [i (range 0 4)]
;;   (conj
;;    (map #(bit-shift-left 2 %) i)
;;    1))
