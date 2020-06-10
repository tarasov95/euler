(ns sicp.queen-future
  (:require [sicp.queen-puzzle :as qp]))

(defn find-games-starting-at [N p]
  (qp/find-all-games [p] N))

(defn count-sequent [N]
  (time (count (mapcat (partial find-games-starting-at N) (range N)))))

(defn count-concur [N]
  (let [rq (map
            (fn [p] (future (find-games-starting-at N p)))
            (range N))]
   (time (count (mapcat deref rq)))))

(let [N 12
      c1 (future (count-sequent N))
      c2 (future (count-concur N))]
  (time (println "c1=" @c1 "; c2=" @c2)))

