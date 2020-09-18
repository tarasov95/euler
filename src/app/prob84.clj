(ns app.prob84
  (:require [clojure.test :as t]
            [lib.seq :as sq]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.string :as st]
            [clojure.core.matrix :as m]
            [clojure.pprint :as pp]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=84

(m/set-current-implementation :vectorz)

(def p-side 1/6)

(def p-double (* 6 (numb/sqr p-side)))

(def p-3double (numb/cub p-double))

(def p-base 1/40)

(def cells [:GO, :A1, :CC1, :A2, :T1, :R1, :B1, :CH1, :B2, :B3, :JAIL, :C1, :U1, :C2, :C3, :R2, :D1, :CC2, :D2, :D3, :FP, :E1, :CH2, :E2, :E3, :R3, :F1, :F2, :U2, :F3, :G2J, :G1, :G2, :CC3, :G3, :R4, :CH3, :H1, :T2, :H2]);

(s/def ::cell #(some (fn [e] (= % e)) cells))

(t/deftest cells-validate
  (t/is (= 40 (count cells))))

(def p-roll6 (mapv #(/ % 36) [1 2 3 4 5 6 5 4 3 2 1]))
(def p-dbl6 (* 6 (numb/sqr 1/6)))
(def sides (into [] (range 1 7)))

(defn throw-dice []
  [(rand-nth sides)
   (rand-nth sides)])

(defn simul-dice [N]
  (->> (range N)
       (mapcat (fn [_] (let [pair (throw-dice)] (sq/conj-if
                                                 [(reduce + pair)]
                                                 (if (= (pair 0) (pair 1)) -1 nil)))))
       (frequencies)
       (map #(let [[i f] %] (vector i (* 100.0 (/ f N)))))
       (into {})))

(t/deftest p-roll6-test
  (let [sim (->> (simul-dice 1000000)
                 (map #(let [[k v] %] [k (int v)]))
                 (into {}))]
    (t/is (and (= (-> p-dbl6 (* 100) int) (sim -1))
               (= (dissoc sim -1)
                  (->> p-roll6
                       (map-indexed #(vector (+ 2 %1) (-> %2 (* 100) int)))
                       (into {})))))))

(defn adjust-jail-prob [c-dst p-base]
  (max 0
       (if (= c-dst :JAIL)
         (min (+ p-base p-dbl6) 1)
         p-base)))

(defn adjust-cell-prob [c-src c-dst p-base]
  (adjust-jail-prob
   c-dst
   (cond
     (= c-src :G2J) (if (= c-dst :JAIL) 1 0)
     (and (#{:CC1 :CC2 :CC3} c-src)
          (#{:JAIL :GO} c-dst)) (+ p-base 1/16)
     (= c-src :CH1) (case c-dst
                      :GO (+ p-base 1/16)
                      :JAIL (+ p-base 1/16)
                      :C1 (+ p-base 1/16)
                      :E3 (+ p-base 1/16)
                      :H2 (+ p-base 2/16) ;;+go back 3 sq
                      :R1 (+ p-base 1/16)
                      :R2 (+ p-base 2/16) ;;+next R +next R
                      :U1 (+ p-base 1/16)
                      p-base)
     (= c-src :CH2) (case c-dst
                      :GO (+ p-base 1/16)
                      :JAIL (+ p-base 1/16)
                      :C1 (+ p-base 1/16)
                      :E3 (+ p-base 1/16)
                      :H2 (+ p-base 1/16)
                      :R1 (+ p-base 1/16)
                      :R3 (+ p-base 2/16) ;;+next R +next R
                      :D3 (+ p-base 1/16) ;;+go back 3 sq
                      :U2 (+ p-base 1/16)
                      p-base)
     (= c-src :CH3) (case c-dst
                      :GO (+ p-base 1/16)
                      :JAIL (+ p-base 1/16)
                      :C1 (+ p-base 1/16)
                      :E3 (+ p-base 1/16)
                      :H2 (+ p-base 1/16)
                      :R1 (+ p-base 3/16) ;;+next R +next R
                      :CC3 (+ p-base 1/16) ;;+go back 3 sq
                      :U2 (+ p-base 1/16)
                      p-base)
     ;; (#{:CC1 :CC2 :CC3} c-dst) (- p-base 2/16)
     ;; (#{:CH1 :CH2 :CH3} c-dst) (- p-base 10/16)
     :else p-base)))

(defn p-row [c-src cells probs]
  (map (fn [c-dst p] (vector c-dst (adjust-cell-prob c-src c-dst p))) cells probs))

(defn probs4row [ix p]
  (concat
   (repeat (+ 2 ix) 0)
   p
   (repeat (- 38 (count p)) 0)))

(t/deftest probs4row-test
  (t/is (= 40 (count (probs4row 0 p-roll6)))))

(defn markov-templ []
  (let [c (into [] cells)]
    (->> c
         (map-indexed (fn [k v]
                        (p-row v c (probs4row k p-roll6))))
         ;; (map (partial map second))
         )))

(defn to-matrix [mt]
  (m/matrix
   ))



(comment (let [m (markov-templ)]
   (->> m
        ;; (map (partial reduce +))
        ;; (map double)
        ;; (map #(vector %1 %2) cells)
        (take-last 1)
        )))

(s/def :dice/no-of-sides number?)
(s/def :roll/double? boolean?)
(s/def :roll/outcome (s/tuple number? :roll/double?))

(defn p-roll-list-all [N]
  (for [x (range 1 (inc N))
        y (range 1 (inc N))]
    [(+ x y) (= x y)]))

(s/fdef p-roll-list-all
  :args (s/cat :N :dice/no-of-sides)
  :ret (s/coll-of :roll/outcome))

(def p-roll-calc
  (memoize
   (fn [N]
     (let [ro (p-roll-list-all 6)
           co (count ro)
           dbl (filter second ro)
           p-dbl (/ (count dbl) co)
           p-dbl3 (numb/cub p-dbl)
           p-roll-full (->> ro
                            (map first)
                            (frequencies)
                            (map #(vector (first %) (/ (second %) co)))
                            (into {}))
           p-roll-no-dbl3 (reduce (fn [z e] (assoc z e (- (z e) p-dbl3)))
                                  p-roll-full
                                  (map first dbl))]
       {:ro ro
        :dbl dbl
        :p-roll-no-dbl3 p-roll-no-dbl3
        :p-dbl p-dbl
        :p-dbl3 p-dbl3
        :p-roll-full p-roll-full}))))

(t/deftest p-roll-calc-test
  (t/is (= 36 (count (:ro (p-roll-calc 6))))))

(def p-cell-calc
  (memoize
   (fn [N]
     (let [p-roll (p-roll-calc N)
           p0 (->> (into (sorted-map) (:p-roll-no-dbl3 p-roll))
                   (map second)
                   (concat [0 0])
                   (into []))]
       (into p0
             (repeat (- (count cells) (count p0)) 0))))))

(s/fdef p-cell-calc
  :args (s/cat :N :dice/no-of-sides)
  :ret (s/coll-of number? :kind vector))

(t/deftest p-cell-calc-test
  (t/is (= 40 (count (p-cell-calc 6)))))

(def proll-row
  (memoize
   (fn [N ix]
     (let [p-cell-base (p-cell-calc N)
           p-dbl (* 6 (:p-dbl3 (p-roll-calc N)))
           cc (count p-cell-base)]
       (->> (range (- ix) (- cc ix))
            (map #(p-cell-base (mod % cc)))
            (map-indexed #(if (= :JAIL (cells %1)) (+ p-dbl %2) %2))
            (into []))))))

(t/deftest proll-row-test
  (t/is (->> cells
             (map-indexed (fn [ix _] (proll-row 6 ix)))
             (map (partial reduce +))
             (every? (partial = 1)))))

(defn ix-cell
  ([pref no]
   (.indexOf cells (keyword (str pref no))))
  ([cell-kwd] (.indexOf cells cell-kwd)))

(defn p-CC [N CC-no]
  (let [ix-cc (ix-cell "CC" CC-no)
        ix-jail (ix-cell :JAIL)
        ix-go (ix-cell :GO)
        pbase (proll-row N ix-cc)]
    (letfn [(p-cc [dest] (if (#{ix-jail ix-go} dest) 1/16 0))]
      (reduce-kv
       (fn [z k v] (conj z (+ (* v 14/16) (p-cc k))))
       []
       pbase))))

(s/def ::CC-no #(> (ix-cell "CC" %) 0))
(s/fdef p-CC
  :args (s/cat :N :dice/no-of-sides :CC-no ::CC-no)
  :ret (s/coll-of number? :kind vector))

(stest/instrument)

(t/deftest p-CC-test
  (t/is
   (->> cells
        (filter #(st/starts-with? (name %) "CC"))
        (map-indexed (fn [ix _] (p-CC 6 (inc ix))))
        (map (partial reduce +))
        (every? (partial = 1)))))

(defn find-cell [ix-start pref]
  (let [cc (count cells)]
    (->> (range ix-start (+ ix-start cc))
         (map #(cells (mod % cc)))
         (filter #(st/starts-with? (name %) pref))
         (first))))

(t/deftest find-cell-test
  (t/is (= :R2 (find-cell (ix-cell :CH1) "R")))
  (t/is (= :R3 (find-cell (ix-cell :CH2) "R")))
  (t/is (= :R1 (find-cell (ix-cell :CH3) "R"))))

(defn ix-back [ix steps]
  (mod (- ix steps) (count cells)))

(t/deftest ix-back-test
  (t/is (= :T1 (cells (ix-back (ix-cell :CH1) 3))))
  (t/is (= :D3 (cells (ix-back (ix-cell :CH2) 3))))
  (t/is (= :CC3 (cells (ix-back (ix-cell :CH3) 3)))))

(defn p-CH [N CH-no]
  (let [ix-ch (ix-cell "CH" CH-no)
        ix [(ix-cell :GO)
            (ix-cell :JAIL)
            (ix-cell :C1)
            (ix-cell :E3)
            (ix-cell :H2)
            (ix-cell :R1)
            (ix-cell (find-cell ix-ch "R"))
            (ix-cell (find-cell ix-ch "R"))
            (ix-cell (find-cell ix-ch "U"))
            (ix-back ix-ch 3)]
        pbase (proll-row N ix-ch)
        fq (frequencies ix)]
    (letfn [(pch [dst] (/ (or (fq dst) 0) 16))]
      (reduce-kv (fn [z k v]
                   (conj z (+ (* v 6/16) (pch k))))
                 []
                 pbase))))

(reduce + (p-CH 6 1))
