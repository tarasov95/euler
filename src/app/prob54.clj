(ns app.prob54
  (:require
   [lib.prime :as prime]
   [clojure.string :as s]
   [clojure.test :as t]
   [lib.numb :as numb]))

;; https://projecteuler.net/problem=54

(defn card-val [v]
  (or ({\T 10 \J 11, \Q 12, \K 13, \A 14} v)
      (- (int v) (int \0))))

(defn card-suite [s]
  (keyword (str s)))

(defn new-card [card]
  (let [[v s] (seq card)]
    {:v (card-val v) :s (card-suite s) :c card}))

(defn new-hand [cards]
  (->> (map new-card cards)
       (sort-by (comp - :v))
       (into [])))

(defn load-data []
  (let [data (slurp "resources/p054_poker.txt")
        rg    (s/split data #"\n")]
    (->> rg
         (map #(s/split % #" "))
         (map #(vector (new-hand (take 5 %))
                       (new-hand (drop 5 %)))))))

(def ^:private data (promise))

(defn all-hands []
  (deliver data (load-data))
  @data)

(defn find-rank [rank-test]
  (->> (all-hands)
       (filter #(some true? (map rank-test %)))))

(defn HighCard
  "High Card: Highest value card."
  [h]
  (:v (first h)))

(t/deftest HighCard-test
  (t/is (= 11 (HighCard (new-hand ["9H" "9S" "9C" "9D" "JH"])))))

(defn n-of-a-kind
  [n h]
  (->> (group-by :v h)
       (filter #(= n (count (second %))))))

(defn OnePair
  "One Pair: Two cards of the same value."
  [h]
  (not-empty (n-of-a-kind 2 h)))

(t/deftest OnePair-test
  (t/is (not (OnePair (new-hand ["AH" "9S" "KC" "TD" "JH"]))))
  (t/is (OnePair (new-hand ["9H" "9S" "KC" "TD" "JH"]))))

(defn TwoPairs
  "Two Pairs: Two different pairs."
  [h]
  (= 2 (count (n-of-a-kind 2 h))))

(t/deftest TwoPairs-test
  (t/is (TwoPairs (new-hand ["AH" "9S" "AC" "9D" "JH"])))
  (t/is (not (TwoPairs (new-hand ["9H" "9S" "KC" "TD" "JH"])))))

(defn ThreeOfAKind
  "Three of a Kind: Three cards of the same value."
  [h]
  (not-empty (n-of-a-kind 3 h)))

(t/deftest ThreeOfAKind-test
  (t/is (ThreeOfAKind (new-hand ["9H" "AS" "9C" "9D" "JH"])))
  (t/is (not (ThreeOfAKind (new-hand ["9H" "AS" "QC" "9D" "JH"])))))

(defn Straight
  "Straight: All cards are consecutive values."
  [h]
  (->> (map #(= (dec (:v %1)) (:v %2))
            h (drop 1 h))
       (every? true?)))

(t/deftest Straight-test
  (t/is (not (Straight (new-hand ["KS" "QS" "TS" "TS" "9S"]))))
  (t/is (Straight (new-hand ["KS" "QS" "JD" "TS" "9S"]))))

(defn Flush
  "Flush: All cards of the same suit."
  [h]
  (let [s (:s (first h))]
    (every? #(= s (:s %)) (rest h))))

(t/deftest Flush-test
  (t/is (not (Flush (new-hand ["KS" "QD" "TS" "TS" "9S"]))))
  (t/is (Flush (new-hand ["KS" "QS" "JS" "TS" "9S"]))))

(defn FullHouse
  "Full House: Three of a kind and a pair."
  [h]
  (and (ThreeOfAKind h)
       (OnePair h)))

(t/deftest FullHouse-test
  (t/is (not (FullHouse (new-hand ["KS" "QD" "TS" "TS" "9S"]))))
  (t/is (FullHouse (new-hand ["KS" "KD" "JS" "JH" "JD"]))))

(defn FourOfAKind
  "Four of a Kind: Four cards of the same value."
  [h]
  (not-empty (n-of-a-kind 4 h)))

(t/deftest FourOfAKind-test
  (t/is (FourOfAKind (new-hand ["9H" "9S" "9C" "9D" "JH"])))
  (t/is (not (FourOfAKind (new-hand ["9H" "9S" "QC" "9D" "JH"])))))

(defn StraightFlush
  "Straight Flush: All cards are consecutive values of same suit."
  [h]
  (if (->> (map #(and (= (:s %1) (:s %2))
                   (= (dec (:v %1)) (:v %2)))
             h (drop 1 h))
           (every? true?))
    (+ 9000000 (:v (first h)))
    nil))

(t/deftest test-StraightFlush
  (t/is (= 0 (count (find-rank StraightFlush))))
  (t/is (StraightFlush (new-hand ["KS" "QS" "JS" "TS" "9S"]))))

(defn RoyalFlush
  "Royal Flush: Ten, Jack, Queen, King, Ace, in same suit."
  [h]
  (if (and (= 14 (-> h first :v))
           (StraightFlush h))
    10000000
    nil))

(t/deftest test-RoyalFlush
  (t/is (RoyalFlush (new-hand ["AH" "KH" "QH" "JH" "TH"])))
  (t/is (not (RoyalFlush (new-hand ["AS" "KH" "QH" "JH" "TH"])))))

