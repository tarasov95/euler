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

