(ns app.prob86
  (:require [clojure.test :as t]
            [lib.seq :as sq]
            [clojure.spec.alpha :as s]
            [app.prob75 :as p75]
            [clojure.spec.test.alpha :as stest]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=86


;; (comment
;;  t1 = [a1 b1 h1]
;;  t2 = [a2 b2 h2]

;;  t1 = [5 b1 h1]
;;  t2 = [a2 3 h2]
;;  b1 + a2 = 6
;;  h1 + h2 ~> int

;;  5^2 + b1^2 = h1^2
;;  a2^2 + 3^2 = h2^2
;; )

(defn hypotenuse [side1 side2]
  (Math/sqrt (+ (numb/sqr side1) (numb/sqr side2))))


(defn list-perim-le [P]
  (p75/bfs-pyth [] conj (partial p75/perim-fits? P)))


(comment
  (list-perim-le (* 2 (+ 5 6 (max (hypotenuse 5 6)
                                  (hypotenuse 3 6))))))

(s/def ::cuboid (s/tuple number? number? number?))

(defn paths [cuboid]
  (let [[a b c] cuboid]
    (->> [[a (+ b c)]
          [b (+ a c)]
          [c (+ a b)]]
         (mapv (partial apply hypotenuse)))))

(s/fdef paths
  :args (s/cat :cuboid ::cuboid)
  :ret (s/coll-of number? :kind vector? :count 3))

(defn has-int-path [cuboid]
  (->> (paths cuboid)
       (apply min)
       (numb/natural?)))

(t/deftest has-int-path-test
  (t/is (has-int-path [3 5 6])))

(defn count-cuboids [N]
  (->> (for [x (range 1 (inc N))
             y (range 1 (inc N))
             z (range 1 (inc N))
             :when (>= x y z)]
         (has-int-path [x y z]))
       (filter true?)
       (count)))

(t/deftest count-cuboids-test
  (t/is (= 2060 (count-cuboids 100)))
  (t/is (= 1975 (count-cuboids 99))))

(defn triang [cuboid]
  (let [[a b c] cuboid]
    (->> [[a (+ b c)]
          [b (+ a c)]
          [c (+ a b)]]
         (mapv #(conj % (apply hypotenuse %))))))

(defn multiples-between [x l u]
  (range (* x (inc (quot l x)))
         (inc (* x (quot u x)))
         x))

(defn quot-between [x l u]
  (range (inc (quot l x))
         (inc (quot u x))))


(defn tmul [t n]
  (mapv (partial * n) t))

(t/deftest tmul-test
  (t/is (= [2 4 6] (tmul [1 2 3] 2))))

(defn non-primitives-between [rg u l]
  (->> rg
       (map (fn [t] (quot-between (first t) u l))))
  ;; (->> rg
  ;;      (map (fn [t] (->> (quot-between (first t) u l)
  ;;                        (map #(tmul t %))))))
  )

(t/deftest quot-between-test
  (quot-between 3 11 13))

(t/deftest non-primitives-between-test
  (t/is (= [[6 8 10]]
           (non-primitives-between [[3 4 5] [5 12 13] [7 24 25] [9 40 41] [11 60 61]] 11 13)))
  (t/is (= [[6 8 10]]
           (non-primitives-between [[3 4 5] [5 12 13]] 5 7))))

(defn count-next-triang [z t]
  (if (>= (count z) 10)
    (reduced z)
    (let [tn (into [] (map int) (sort t))
          tl (get z (dec (count z)))]
      (non-primitives-between z (first tl) (first tn))
      (conj z tn))))

;; (p75/bfs-pyth [] count-next-triang (constantly true))

;; consider e.g. triplet [3 4 5]
;; a=3,b+c=4 or a=4, b+c=3

(defn paths2 [cuboid]
  (let [[a b c] cuboid]
    (->> [[a (+ b c)]
          [b (+ a c)]
          [c (+ a b)]]
         (mapv #(vector (apply hypotenuse %) %))
         (filter #(numb/natural? (first %))))))

(comment (->> [[3 2 2]
       [3 1 3]
       [4 1 2]]
      (map paths2)))

(comment (->> [[5 11 1]
       [5 10 2]
       [12 4 1]]
      (map paths2)))

(defn matching-triple? [N t]
  (or (and (<= (first t) N)
          (<= (second t) (* 2 N)))
     (and (<= (second t) N)
          (<= (first t) (* 2 N)))))

(defn list-primitive-triples [N]
  (p75/bfs-pyth []
                (fn [z t] (conj z (into [] (map long) t)))
                (partial matching-triple? N)))

(t/deftest list-primitive-triples-verify-ordered
  (t/is (empty?
         (->> (list-primitive-triples 10000)
                     (filter #(let [[a b h] %]
                                (or (> a h)
                                    (> b h))))))))

(defn derive-from-triple [N t]
  (->> (range)
       (drop 1)
       (map #(tmul t %))
       (take-while (partial matching-triple? N))))

;; [19 180 181]
;; [19 100+80]

(t/deftest derive-from-triple-test
  (t/is (= [[3 4 5] [6 8 10]] (derive-from-triple 10 [3 4 5]))))

(defn count-cuboids-in-triple [N t]
  (let [[a b] t]
    (+ (* (if (> a N) 0 1) (quot (min N b) 2))
       (* (if (> b N) 0 1) (quot (min N a) 2)))))

(t/deftest count-cuboids-in-triple-test
  (t/is (= 3 (count-cuboids-in-triple 100 [3 4 5])))
  (t/is (= 7 (count-cuboids-in-triple 100 [6 8 10])))
  (t/is (= 5 (count-cuboids-in-triple 10 [5 12 13]))))

(comment (let [N 99]
   (->> (list-primitive-triples N)
        (map (partial derive-from-triple N))
        ;; (map (partial count-cuboids-in-triple N))
        ;; (reduce +)
        )))

(defn list-cuboids-in-triple [N t]
  (letfn [(lstcub [a b]
            (let [U (min N (dec b))]
              (->> (range (- b U) (inc (quot b 2)))
                   (map #(with-meta
                           (vector a (- b %) %)
                           {:t t}))
                   (filter #(apply >= %))
                   (filter has-int-path))))]
    (let [[a b] t]
      (->> (concat
            (if (> a N) nil (lstcub a b))
            (if (> b N) nil (lstcub b a)))))))

(t/deftest list-cuboids-in-triple-test
  (t/is (= [[3 3 1] [3 2 2] [4 2 1]] (list-cuboids-in-triple 20 [3 4 5])))
  (t/is (= [[5 10 2] [5 9 3] [5 8 4] [5 7 5] [5 6 6]] (list-cuboids-in-triple 20 [5 12 13])))
  (t/is (= [[5 10 2] [5 9 3] [5 8 4] [5 7 5] [5 6 6]] (list-cuboids-in-triple 10 [5 12 13]))))

(defn list-cuboids-v1 [N]
  (->> (for [x (range 1 (inc N))
             y (range 1 (inc N))
             z (range 1 (inc N))
             :when (>= x y z)]
         [x y z])
       (filter has-int-path)))

(defn list-cuboids-v2 [N]
  (->> (list-primitive-triples N)
       (mapcat (partial derive-from-triple N))
       (mapcat (partial list-cuboids-in-triple N))))


(defn verify-list-cuboids-v2 [N]
  (let [z0 (reduce (fn [z e] (assoc z e 1)) {} (list-cuboids-v1 N))
        z1 (list-cuboids-v2 N)]
    [(count z0)
     (count z1)
     (->> z1
          (filter #(not (z0 %)))
          (map #(vector % (meta %) (has-int-path %))))]))

(t/deftest list-cuboids-v2-test
  (t/is (= [2060 2060 (list)] (verify-list-cuboids-v2 100)))
  (t/is (= [1975 1975 (list)] (verify-list-cuboids-v2 99))))

;;TODO: implement binary search over M
(defn solve []
  (->> [1818 1817]
       (pmap #(count (list-cuboids-v2 %)))))
