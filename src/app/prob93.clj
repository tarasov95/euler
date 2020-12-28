(ns app.prob93
  (:require [clojure.test :as t]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [lib.numb :as numb]
            [lib.seq :as seq]
            [clojure.spec.test.alpha :as stest]))

;; https://projecteuler.net/problem=93

(defn ccomb [n k]
  (/ (numb/fact n) (* (numb/fact k) (numb/fact (- n k)))))

(def inf "infinity")

(defn div [x y]
  (if (zero? y) inf
      (/ x y)))

(def ops ["+" "*" "-" "/"])
(def op (->> [+ * - div]
             (map (fn [name fun]
                    (with-meta
                      (fn [x y]
                        (if (or (= x inf)
                                (= y inf))
                          inf
                          (fun x y)))
                      {:name name}))
                  ops)))

(def d09 (range 0 10))

(defn gen-ops-sym []
  (for [o1 ops
        o2 ops
        o3 ops]
    (fn [x1 x2 x3 x4]
      (str "(" o3
           "(" o2
           "(" o1 " " x1 " " x2 ")" " " x3 ")" " " x4 ")"))))

(defn substitute [templ vals]
  (->> (str/split templ #"\b")
       (map (fn [t] (if (vals t)
                      (vals t)
                      t)))
       (reduce str)))

(defmacro oz [expr]
  (let [sypr (str expr)]
    `(with-meta
       (vector ~expr)
       {:expr (substitute ~sypr
                          {"x1" ~'x1
                           "x2" ~'x2
                           "x3" ~'x3
                           "x4" ~'x4
                           "o1" (:name (meta ~'o1))
                           "o2" (:name (meta ~'o2))
                           "o3" (:name (meta ~'o3))})
        :src ~sypr})))

(defn gen-ops []
  (->> (for [o1 op
             o2 op
             o3 op]
         (list
          (fn [x1 x2 x3 x4] (oz (o3 (o2 (o1 x1 x2) x3) x4)))
          (fn [x1 x2 x3 x4] (oz (o3 (o2 (o1 x2 x1) x3) x4)))
          (fn [x1 x2 x3 x4] (oz (o3 (o2  x3 (o1 x1 x2)) x4)))
          (fn [x1 x2 x3 x4] (oz (o3 (o2  x3 (o1 x2 x1)) x4)))
          (fn [x1 x2 x3 x4] (oz (o3  x4 (o2 (o1 x1 x2) x3))))
          (fn [x1 x2 x3 x4] (oz (o3  x4 (o2 (o1 x2 x1) x3))))
          (fn [x1 x2 x3 x4] (oz (o3  x4 (o2  x3 (o1 x1 x2)))))
          (fn [x1 x2 x3 x4] (oz (o3  x4 (o2  x3 (o1 x2 x1)))))))
       (reduce concat)
       (into (list))))

(defn apply-ops [ops arg]
  (let [el (fn [vect] (vect 0))]
    (->> ops
         (map #(apply % arg))
         (filter (comp (partial not= inf) el))
         (filter #(> (el %) 0))
         (filter (comp numb/natural? el))
         (map #(-> % el long vector (with-meta (meta %))))
         (distinct)
         (sort))))

(defn gen-ints
  ([digs] (gen-ints (gen-ops) digs))
  ([ops digs]
   (let [args (seq/permut digs)]
     (with-meta
       (->> args
            (mapcat (partial apply-ops ops))
            (distinct)
            (sort))
       {:set digs}))))

(defn select-conseq [rg]
  (with-meta
    (->> rg
        (map #(into %2 %1) (map vector (drop 1 (range))))
        (take-while (partial apply =)))
    (meta rg)))

;; (select-conseq (gen-ints [4 6 8 9]))

(defn gen-sets-of-4 []
  (for [d1 d09
        d2 d09
        d3 d09
        d4 d09
        ;; :when (< d1 d2 d3 d4)
        :when (and (< d1 d2)
                   (< d2 d3)
                   (< d3 d4))]
    [d1 d2 d3 d4]))

(defn gen-sets []
  (for [d1 d09
        d2 d09
        d3 d09
        d4 d09
        ;; :when (< d1 d2 d3 d4)
        :when (and (not= d1 d2)
                   (not= d1 d3)
                   (not= d1 d4)
                   (not= d2 d3)
                   (not= d2 d4)
                   (not= d3 d4))]
    [d1 d2 d3 d4]))

(defn solve2 []
  (let [ops (gen-ops)]
    (->> (gen-sets)
         (map #(vector (apply str (sort %))
                       (apply-ops ops %)))
         (filter #(-> % second not-empty))
         (reduce (fn [z e]
                   (let [[k v] e]
                     (assoc z k
                            (into (or (z k)
                                      (sorted-set)) v)))) {})
         (map (fn [e]
                (let [[k v] e]
                  [k (select-conseq v)])))
         (apply max-key #(-> % second count)))))

(t/deftest permut-gen-test
  (t/is
   (= (into #{} (seq/permut [2 3 8 9]))
      (->> (gen-sets)
           (filter #(= #{2 3 8 9} (into #{} %)))
           (into #{})))))

(t/deftest gen-ints-test
  (t/is (= 33
           (->> (gen-ints [2 3 8 9])
                (select-conseq)
                (count)))))

(defn explain1258 []
  (->> (seq/permut [1 2 5 8])
       (mapcat (partial apply-ops (gen-ops)))
       (distinct)
       (sort)
       (select-conseq)
       (map #(vector (% 0) (-> % meta :expr)))))

(defn solve1 []
  (let [ops (gen-ops)
        z (->> (gen-sets-of-4)
               ;; (filter (partial = [1 2 3 4]))
               (map (partial gen-ints ops))
               (map select-conseq)
               (apply max-key count))]
    [(count z)
     (meta z)]))

(t/deftest solve-test
  (t/is (= [51 {:set [1 2 5 8]}] (solve1))))
