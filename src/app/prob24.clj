(ns app.prob24)

;; https://projecteuler.net/problem=24


(defn perm [d]
  (if (empty? (rest d))
    (list d)
    (mapcat (fn [e]
              (let [d2 (filter #(not= % e) d)]
                (map #(conj % e) (perm d2))))
            d)))

(defn print-perm [d]
  (doseq [r (map-indexed #(vector (inc %1) %2) (perm d))]
    (println "#" (first r) ": " (second r))))

(print-perm [0 1 2 3])
