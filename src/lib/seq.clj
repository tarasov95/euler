(ns lib.seq)

(defn conj-each [col e]
  (map #(conj % e)
       col))

(defn permut [rg]
  (if (empty? (rest rg))
    (list rg)
    (->> rg
         (mapcat (fn [e]
                   (conj-each
                    (permut (filter #(not= % e) rg))
                    e))))))

(defn palindrome? [rg]
  (let [cnt (count rg)]
    (case cnt
      (0 1) true
      (2 3) (= (first rg) (last rg))
      (and (= (first rg) (last rg))
           (recur (subvec rg 1 (dec cnt)))))))
