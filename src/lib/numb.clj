(ns lib.numb)

(defn pow-int
  "x to the power of n; n must be an integer"
  ([x n] (pow-int x n 1))
  ([x n z]
   (cond (<= n 1) (* x z)
         (even? n) (recur (* x x) (quot n 2) z)
         :else (recur x (dec n) (* x z)))))

(defn num2dig
  "returns array of digits of n in numberal system with base B"
  [B n]
  (if (= n 0)
    []
    (conj
     (num2dig B (quot n B))
     (mod n B))))

(defn dig2num
  "returns number from array of it's digits in numberal system with base"
  ([base digits] (dig2num 0 base digits))
  ([z base digits]
   (if (empty? digits)
     z
     (recur (+ (first digits) (* base z)) base (rest digits)))))

(defn gcd
  "greates common divider"
  [n1 n2]
  (cond
    (< n1 0)  1
    (= n1 n2) n1
    (> n1 n2) (recur (- n1 n2) n2)
    :else (recur (- n2 n1) n1)))

(defn fact
  "factorial of the argument"
  ([n] (if (= n 0)
         1
         (fact 1 n)))
  ([z n]
   (if (= 1 n)
     z
     (recur (* z n) (dec n)))))

(defn log-int
  "floor of pow <~> base^pow==order of n"
  [base n]
  (int (Math/floor (/ (Math/log n) (Math/log base)))))

(defmacro gen-lgpw10
  "generates a cond to return (pow-int 10 (log-int 10 arg)) supports up to max-pow of 10"
  [max-pow arg]
  (let [cnd (mapcat
             (fn [pw] (list `(< ~arg ~pw) (/ pw 10)))
             (map #(pow-int 10 %) (range 1 (inc max-pow))))]
    `(cond ~@cnd)))

(defn dig-mask-count-repeats [n]
  (if (= 0 n)
    [(int 1) 0]
    (loop [mask (int 0)
           q n
           c 0]
      (let [d (mod q 10)
            f (bit-test mask d)]
        (if (= 0 q) [mask c]
            (recur (bit-set mask d)
                   (quot q 10)
                   (if f (inc c) c)))))))

(defn dig-mask [n]
  (first (dig-mask-count-repeats n)))

(defn any-dig-in?
  "checks if the number contains any digits encoded in the bit mask"
  [mask n]
  (if (= 0 n)
    (bit-test mask 0)
    (loop [q n]
      (let [d (mod q 10)]
        (cond
          (= 0 q) false
          (bit-test mask d) true
          :else (recur (quot q 10)))))))

(defn all-dig-diff?
  "checks if all digits are different, supports numbers up to 64 decimal digits long"
  [n]
  (loop [mask (long 0)
         q n]
    (let [d (mod q 10)]
      (cond
        (= 0 q) true
        (bit-test mask d) false
        :else (recur (bit-set mask d) (quot q 10))))))

(defn natural? [x]
  (< (- x (Math/floor x)) 0.00000001))

(defn palindrome?
  ([n] (palindrome? 1 0 n))
  ([pw10 k n]
   (if (= 0 (quot n pw10))
     (or
      (= n k)
      (= n (quot k 10)))
     (recur
      (* 10 pw10)
      (+ (* 10 k) (mod n 10))
      (quot n 10)))))

(defn reverse
  ([n] (reverse 0 n))
  ([z n]
   (if (= 0 n)
     z
     (recur (+ (* 10 z) (mod n 10))
            (quot n 10)))))
