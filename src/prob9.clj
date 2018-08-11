(ns prob9)

(def N 1000)

(defn is-triple [a b c]
  (= (* c c ) (+ (* a a) (* b b))))

(let [rg (range 1 N)]
  (for [b rg a rg
        :let [c (- N (+ a b))]
        :while (< a b)
        :when (and (< b c) (is-triple a b c))]
    {:triple [a b c]
     :product (* a b c)
     :sum (+ a b c)}))
