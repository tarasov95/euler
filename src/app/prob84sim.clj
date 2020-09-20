(ns app.prob84sim
  (:require [clojure.test :as t]
            [lib.seq :as sq]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.string :as st]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.linear :as ml]
            [clojure.pprint :as pp]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=84

(def cells [:GO, :A1, :CC1, :A2, :T1, :R1, :B1, :CH1, :B2, :B3, :JAIL, :C1, :U1, :C2, :C3, :R2, :D1, :CC2, :D2, :D3, :FP, :E1, :CH2, :E2, :E3, :R3, :F1, :F2, :U2, :F3, :G2J, :G1, :G2, :CC3, :G3, :R4, :CH3, :H1, :T2, :H2]);

(s/def ::cell #(some (fn [e] (= % e)) cells))

(t/deftest cells-validate
  (t/is (= 40 (count cells))))

(def count-cells (count cells))

(def ^:dynamic *sides* (into [] (range 1 7)))

(defn throw-dice []
  [(rand-nth *sides*)
   (rand-nth *sides*)])

(defmacro cell-index [kwd]
  (let [ix (reduce concat (map-indexed (fn [ix kw] (list kw ix)) cells))]
    `(case ~kwd ~@ix)))

(t/deftest cell-index-test
  (t/is
   (let [x :JAIL]
     (= (.indexOf cells x)
        (cell-index x)))))

(defn ix-cell
  ([pref no]
   (.indexOf cells (keyword (str pref no))))
  ([cell-kwd] (cell-index cell-kwd)))

;; (defn p-CC
;;   ([N CC-no _] (p-CC N (ix-cell "CC" CC-no)))
;;   ([N ix-cc]
;;    (let [ix-jail (ix-cell :JAIL)
;;          ix-go (ix-cell :GO)
;;          pbase (proll-row N ix-cc)]
;;      (letfn [(p-cc [dest] (if (#{ix-jail ix-go} dest) 1/16 0))]
;;        (reduce-kv
;;         (fn [z k v] (conj z (+ (* v 14/16) (p-cc k))))
;;         []
;;         pbase)))))

(defn find-cell [ix-start pref]
  (let [cc (count cells)]
    (->> (range ix-start (+ ix-start cc))
         (map #(cells (mod % cc)))
         (filter #(st/starts-with? (name %) pref))
         (first))))

(defn ix-back [ix steps]
  (cells (mod (- ix steps) count-cells)))

;; (defn p-CH
;;   ([N CH-no _] (p-CH N (ix-cell "CH" CH-no)))
;;   ([N ix-ch]
;;    (let [ix [(ix-cell :GO)
;;              (ix-cell :JAIL)
;;              (ix-cell :C1)
;;              (ix-cell :E3)
;;              (ix-cell :H2)
;;              (ix-cell :R1)
;;              (ix-cell (find-cell ix-ch "R"))
;;              (ix-cell (find-cell ix-ch "R"))
;;              (ix-cell (find-cell ix-ch "U"))
;;              (ix-back ix-ch 3)]
;;          pbase (proll-row N ix-ch)
;;          fq (frequencies ix)]
;;      (letfn [(pch [dst] (/ (or (fq dst) 0) 16))]
;;        (reduce-kv (fn [z k v]
;;                     (conj z (+ (* v 6/16) (pch k))))
;;                   []
;;                   pbase)))))

(defn cc-cards []
  (->> (concat [:GO :JAIL] (repeat 14 nil))
       (shuffle)
       (into [])))

(t/deftest cc-cards-test
  (t/is (= 16 (count (cc-cards)))))

(defn ch-cards []
  (->> (concat [:GO :JAIL :C1 :E3 :H2 :R1 :next-R :next-R :next-U :back-3]
             (repeat 6 nil))
       (shuffle)
       (into [])))

(t/deftest ch-cards-test
  (t/is (= 16 (count (ch-cards)))))

(defn pick-a-card [cards]
  (let [c (first cards)]
    [c (conj (subvec cards 1) c)]))

(t/deftest pick-a-card-test
  (let [cards0 (ch-cards)]
    (let [[c cards1] (pick-a-card cards0)]
      (t/is (and (= 16 (count cards1))
                 (= (last cards1) c))))))

(defn init-game [N]
  (transient
   {:N N
    :z (transient (assoc
                   (->> cells (map #(vector % 0)) (into {}))
                   :GO 1))
    :cnt 1
    :dbl 0
    :last :GO
    :ch-cards (ch-cards)
    :cc-cards (cc-cards)}))

(t/deftest init-game-test
  (t/is (and (= 40 (count (:z (init-game 10))))
             (= 1 (:GO (:z (init-game 10))))
             (= 0 (:JAIL (:z (init-game 10)))))))

(defn done? [game]
  (>= (:cnt game) (:N game)))

(defn inc-cnt! [game]
  (assoc! game :cnt (inc (:cnt game))))

(t/deftest inc-cnt-test
  (let [game (init-game 10)]
    (t/is (= (:cnt (inc-cnt! game))
             (:cnt game)))))

(defn conjz! [game cell]
  (let [fq (:z game)]
    (assoc! fq cell (inc (fq cell)))
    (assoc! game :last cell)
    (inc-cnt! game)
    game))

(t/deftest conjz!-test
  (let [g (init-game 10)
        z (conjz! g :JAIL)]
    (t/is (= 1 (:JAIL (:z g))))
    (t/is (= :JAIL (:last g)))
    (t/is (= 2 (:cnt g)))))

(defn resolve-card [base card]
  (case card
    :next-R (find-cell (ix-cell base) "R")
    :next-U (find-cell (ix-cell base) "U")
    :back-3 (ix-back (ix-cell base) 3)
    card))

(defn make-move [game start dist]
  (let [ix-src (ix-cell start)
        ix-dst (mod (+ ix-src dist) count-cells)
        dst (cells ix-dst)]
    (cond
      (= dst :G2J) (conjz! game :JAIL)
      (#{:CC1 :CC2 :CC3} dst) (let [[c cards1] (pick-a-card (:cc-cards game))
                                    g1 (assoc! game :cc-cards cards1)]
                                (if (nil? c)
                                  (conjz! g1 dst)
                                  (recur g1 c 0)))
      (#{:CH1 :CH2 :CH3} dst) (let [[c cards1] (pick-a-card (:ch-cards game))
                                    g1 (assoc! game :ch-cards cards1)]
                                (if (nil? c)
                                  (conjz! g1 dst)
                                  (recur g1 (resolve-card dst c) 0)))
      :else (conjz! game dst))))

(t/deftest make-move-test
  (let [g0 (init-game 10)
        g1 (make-move g0 :G2 4)]
    [(persistent! (:z g1))
     (:ch-cards g1)
     (:last g1)]))

(defn reset-double [game]
  (assoc! game :dbl 0))

(defn inc-double [game]
  (assoc! game :dbl (inc (:dbl game))))

(defn record-double [game dist]
  (let [dbl (inc (:dbl game))]
    (if (= 3 dbl)
      (conjz! (reset-double game) :JAIL)
      (make-move (inc-double game) (:last game) dist))))

(defn play [game]
  (if (done? game)
    (persistent! (:z game))
    (let [[r0 r1] (throw-dice)]
      (if (= r0 r1)
        (recur (record-double game (+ r0 r1)))
        (recur (make-move (reset-double game) (:last game) (+ r0 r1)))))))

(defn solve-for-4 []
  (binding [*sides* (range 1 5)]
   (let [z (play (init-game 10000000))]
     (->> z
          (sort-by second)
          (take-last 3)
          (map #(vector (ix-cell (first %)) %))))))
