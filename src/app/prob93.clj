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

(def op (->> [+ * - div]
             (map (fn [fun]
                    (fn [x y]
                      (if (or (= x inf)
                              (= y inf))
                        inf
                        (fun x y)))))))

(def d09 (range 0 10))

(def ops ["+" "*" "-" "/"])

(defn gen-ops-sym []
  (for [o1 ops
        o2 ops
        o3 ops]
    (fn [x1 x2 x3 x4]
      (str "(" o3
        "(" o2
         "(" o1 " " x1 " " x2")" " " x3 ")" " " x4 ")"))))

(defn gen-ops []
  (->> (for [o1 op
             o2 op
             o3 op]
         (list
          (fn [x1 x2 x3 x4] (o3 (o2 (o1 x1 x2) x3) x4))
          (fn [x1 x2 x3 x4] (o3 (o2 (o1 x2 x1) x3) x4))
          (fn [x1 x2 x3 x4] (o3 (o2  x3 (o1 x1 x2)) x4))
          (fn [x1 x2 x3 x4] (o3 (o2  x3 (o1 x2 x1)) x4))
          (fn [x1 x2 x3 x4] (o3  x4 (o2 (o1 x1 x2) x3)))
          (fn [x1 x2 x3 x4] (o3  x4 (o2 (o1 x2 x1) x3)))
          (fn [x1 x2 x3 x4] (o3  x4 (o2  x3 (o1 x1 x2))))
          (fn [x1 x2 x3 x4] (o3  x4 (o2  x3 (o1 x2 x1))))))
       (reduce concat)
       (into (list))))

(defn apply-ops [ops arg]
  (->> ops
       (map #(apply % arg))
       (filter (partial not= inf))
       (filter #(> % 0))
       (filter numb/natural?)
       (map long)
       (distinct)
       (sort)))

(defn gen-ints
  ([digs] (gen-ints (gen-ops)))
  ([digs ops]
   (let [args (seq/permut digs)]
     (with-meta
       (->> args
            (mapcat (partial apply-ops ops)))
       {:set digs}))))

(defn select-conseq [rg]
  (with-meta
    (->> rg
        (map vector (drop 1 (range)))
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
                   (< d3 d4))
        ]
    [d1 d2 d3 d4]))

(defn solve1 []
  (let [ops (gen-ops)
        z (->> (gen-sets-of-4)
              ;; (filter (partial = [1 2 3 4]))
               (map #(gen-ints % ops))
               (map select-conseq)
               (apply max-key count))]
    [(count z)
     (meta z)]))

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
                   (not= d3 d4))
        ]
    [d1 d2 d3 d4]))

(defn solve2 []
  (let [ops (gen-ops)]
    (->> (gen-sets)
         (map #(vector (apply str (sort %))
                       (apply-ops ops %)))
         (filter #(-> % second not-empty))
         (reduce (fn[z e]
                   (let [[k v] e]
                     (assoc z k
                            (into (or (z k)
                                      (sorted-set)) v)))) {})
         (map (fn [e]
                (let [[k v] e]
                  [k (select-conseq v)])))
         ;; (filter #(-> % first (= "1234")))
         
         ;; (filter #(-> % first (= "2389")))
         (apply max-key #(-> % second count))
         ;; (sort-by #(-> % second count))
         )))

(solve2)
