(ns sodoku
  (:require [clojure.core.logic.arithmetic :as arithmetic])
  (:use [clojure.core.logic :rename {fact logic-fact facts logic-facts}]
        midje.sweet))

(def hint-board-1
    [[6 0 0  1 0 8  2 0 3]
     [0 2 0  0 4 0  0 9 0]
     [8 0 3  0 0 4  5 0 0]

     [5 0 4  6 0 7  0 0 9]
     [0 3 0  0 0 0  0 5 0]
     [7 0 0  8 0 3  1 0 2]

     [0 0 1  7 0 0  9 0 6]
     [0 8 0  0 3 0  0 2 0]
     [3 0 2  9 0 4  0 0 5]])

(defn empty-boardo []
  (vec (for [r (range 9)]
         (vec (for [c (range 9)]
                (lvar))))))

(defn unify-with-nonzero-hint [lv hint]
  (if (= hint 0) s# (== lv hint)))

(defn sodoku-board-from-hintso [board hints]
  (everyg
    unify-with-nonzero-hint
    (map vector
         (apply concat hints)
         (apply concat board))))

(defn 
  [ ]

  )

(defn solve-sodoku [hints]
  (first
    (run 1 [q]
         (let [board (empty-boardo)]
           (all
             (== q board)
             (unify-board-with-hintso board hints)
             (everyg )

             )))))

