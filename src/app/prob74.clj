(ns app.prob74
  (:require [clojure.test :as t]
            [lib.prime :as prime]
            [clojure.core.reducers :as r]
            [lib.numb :as numb]))

;; https://projecteuler.net/problem=74

(defmacro gen-fact10 [n]
  (let [cnd (mapcat
             #(list (list '= n %) (numb/fact %))
             (range 0 10))]
    `(cond  ~@cnd)))

(defn fact10 [x] (gen-fact10 x))

(t/deftest fact10-test
  (t/is (= 1 (fact10 0)))
  (t/is (= 24 (fact10 4))))

(defn fact-dig [& digs]
  (transduce (map fact10) +  digs))

(t/deftest fact-dig-test
  (t/is (= 1454 (fact-dig 3 6 3 6 0 1)))
  (t/is (= 1454 (fact-dig 3 6 3 6 0 0)))
  (t/is (= 145 (fact-dig 1 4 5))))

(defn reduce-numb
  ([n fun val] (reduce-numb 10 n fun val))
  ([B n fun val]
   (loop [y (quot n B)
          z (fun val (mod n B))]
     (if (= y 0)
       z
       (recur (quot y B)
              (fun z (mod y B)))))))

(t/deftest reduce-numb-test
  (t/is (= (reverse [1 2 3 4]) (reduce-numb 1234 conj []))))

(defn fact-numb [n]
  (reduce-numb n #(+ %1 (fact10 %2)) 0))

(t/deftest fact-numb-test
  (t/is (= 145 (fact-numb 145))))

(defn count-distinct [n]
  (loop [hs #{n}
         nn (fact-numb n)
         cnt 1]
    (if (hs nn)
      cnt
      (recur (conj hs nn)
             (fact-numb nn)
             (inc cnt)))))

(t/deftest count-distinct-test
  (t/is (= 3 (count-distinct 169)))
  (t/is (= 4 (count-distinct 916))) ;;<< 169 and 916 have different counts!
  (t/is (= 5 (count-distinct 96)))
  (t/is (= 5 (count-distinct 69))))

(t/deftest count-slow-test
  (t/is (= 402 (time (->> (range 0 (long 1e6))
                          (pmap count-distinct)
                          (filter (partial = 60))
                          (count))))))

(defn reduce-fact-chain [n fun val]
  (loop [hs #{n}
         nn (fact-numb n)
         z  val]
    (if (hs nn)
      z
      (recur (conj hs nn)
             (fact-numb nn)
             (fun z nn)))))

;;a cached cycle record can be used to calculate actual length of the cycle, but it can be used to cut off candidates by the max length of the cycle
;; 

(t/deftest reduce-fact-chain-test
  (t/is (= [69 363600 1454 169 363601] (reduce-fact-chain 69 conj [69])))
  (t/is (= [916 363601 1454 169] (reduce-fact-chain 916 conj [916])))
  (t/is (= [169 363601 1454] (reduce-fact-chain 169 conj [169]))))
