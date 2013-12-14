(ns sodoku
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic.arithmetic :as arithmetic]
            [clojure.core.logic.fd :as fd])
  (:use [clojure.core.logic :rename {fact logic-fact facts logic-facts}]
        utils
        midje.sweet))

(def hint-board-0
    [6 0 0  0 0 0  2 0 3
     0 2 0  0 0 0  0 9 0
     8 0 3  0 0 0  0 0 0

     5 0 4  0 0 0  0 0 9
     0 3 0  0 0 0  0 5 0
     7 0 0  0 0 0  0 0 2

     0 0 1  0 0 0  0 0 6
     0 8 0  0 0 0  0 0 0
     3 0 2  0 0 0  0 0 5])

(def hint-board-1
    [6 0 0  1 0 8  2 0 3
     0 2 0  0 4 0  0 9 0
     8 0 3  0 0 4  5 0 0

     5 0 4  6 0 7  0 0 9
     0 3 0  0 0 0  0 5 0
     7 0 0  8 0 3  1 0 2

     0 0 1  7 0 0  9 0 6
     0 8 0  0 3 0  0 2 0
     3 0 2  9 0 4  0 0 5])

(defn print-board
  [board]
  (doseq [xs (partition 9 board)]
    (println xs)))

(defn empty-boardo []
  (repeatedly 81 lvar))

; (defne anyg-emptyo
;   "Pseudo-relation!"
;   [[h & t]]
;   (conde
;     (emptyo h)
;     (if t (any-emptyo t) u#)))
; 
; (defn everyg-mapish
;   "This tweak of everyg takes a veriable number of collections
;   It stops when the first collection is empty/."
;   [rel & colls]
;   (let [colls-cnt (count colls)
;         firsts (repeatedly colls-cnt lvar)
;         rests (repeatedly colls-cnt lvar)]
;   (conde
;     (anyg-emptyo colls)
;     (apply rel (map firsto more))
; 
;     )))
;
(defn unify-board-with-hintso-0 [hints board]
  (everyg
    (fn [[h v]] (== h v))
    (for [[h _ :as hv] (map vector hints board )
          :when (not (= h 0))] hv)))

(defn unify-non-zero-with-hinto
  [hint v]
  (conde
    ((== hint 0) s#)
    ((== hint v) (fd/!= v 0))))

(defne unify-board-with-hintso
  "TODO relational extension that takes
  a rel argument and a variable # of vars"
  [hints board]
  (['() '()] s#)
  ([[hh . th ] [hb . tb ]]
   (unify-non-zero-with-hinto hh hb)
   (unify-board-with-hintso th tb)))


(comment
  (defn group-board-to-squares-0
    [board]
    (let [grouped-inds ]
      (for [row grouped-inds]
        (for [i row]
          (nth board i)))))

  (defn group-board-to-squares-1
    [board]
    (let [grouped-inds ]
      (for [row grouped-inds]
        (for [i row]
          (nth board i))))))

(defn drop-take
  [d t coll]
  (->> coll
      (drop d)
      (take t)))

(defn group-board-to-squares
  [board]
  (for [br (range 3)
        bc (range 3)
        :let [sq-start (drop (+ (* br 27) (* bc 3)) board)]]
    (mapcat #(drop-take % 3 sq-start) [0 9 18])))

(defn sodoku-ruleso
  "Specify a complete and winning sodoku board"
  [board]
  (let [rows (partition 9 board)
        cols (map (partial take-nth 9)
                  (take 9 (iterate rest board)))
        sqs (group-board-to-squares board)
        num-dom (apply fd/domain (range 1 10)) ]
    ; I think this calls domain every time the lambda is called
    ; is that necessary? correct?
    (all
      (everyg fd/distinct cols)
      (everyg fd/distinct sqs)
      (everyg fd/distinct rows)
      (everyg #(fd/in % num-dom) board))))

(defn 
  sodoku-rules-with-hintso
  [hints board]
  (all
    (unify-board-with-hintso hints board)
    (sodoku-ruleso board)))

(defn solve-sodoku [hints]
  (let [res 
        (first
          (run 1 [q]
               (let [board (empty-boardo)]
                 (all
                   (== q board)
                   (sodoku-rules-with-hintso hints board)))))]
    (print-board res)
    res))

; (solve-sodoku hint-board-1)
; (fact (solve-sodoku hint-board-0) => seq?)

(fact
  (run 1 [q] (reduceg fd/+ 0 [ 1 q 3] 6)) => [2])

(fact
  (run 1 [q] (reduceg fd/+ 0 [ 1 2 3] q)) => [6])

(fact
  (run 1 [q] (reduceg fd/+ q [ 1 2 3] 10)) => [4])

(defne single-mapg
  "Single-map, TODO extend to more input collections"
  [rel as os]
  ([_ [] _] s#)
  ([_ _ []] s#)
  ([_ [ah . at] [oh . ot]]
   (all (rel ah oh)
        (single-mapg rel at ot))))

(defne anyg
  [rel coll]
  ([_ []] u#)
  ([_ [h . t]]
   (conde
     [(rel h) s#]
     [(anyg rel t) s#])))

(defn mapg
  "Single-map, TODO extend to more input collections"
  [rel & colls]
  (conde
    ; map stops when one of the input collections is empty
    [(anyg emptyo (drop-last 1 colls)) (emptyo (last colls))]
    ; Colls is a collection of lvars or collections of lvars
    ; Not an lvar
    [(let [firsts (repeatedly (count colls) lvar)
           rests (repeatedly (count colls) lvar)]
       (all 
         (single-mapg firsto colls firsts)
         (single-mapg resto colls rests)
         (apply rel firsts)
         (apply mapg rel rests)))]))

(defn inco [a o] (fd/+ 1 a o))

(fact
  (run 1 [q]
       (fresh [a b c]
              (== q [a b c])
              (mapg inco [a 2 c] [9 b 6]))) => [[8 3 5]])

(fact
  (run 1 [q]
       (fresh [a b c]
              (== q [a b c])
              (mapg inco [0 1 2] q))) => [[1 2 3]])

(fact
  (run 1 [q]
       (fresh [a b c]
              (== q [a b c])
              (mapg fd/+ [0 1 2] [10 9 8] q))) => [[10 10 10]])

(fact
  (run 1 [q]
       (fresh [a]
              (mapg fd/+ a [] q))) => [[]])

(fact
  (run 1 [q]
       (mapg fd/+ [0 1 2] [10 q 8] [10 10 10])) => [9])

(fact
  (run 1 [q]
       (fresh [xs]
              (== xs [[:a] [:b] [:c] q])
              (my-anyg emptyo xs))) => [[]])

(defne filterg
  "Built to work with relations like nilo or emptyo"
  [rel xs os]
  ([_ [] []] s#)
  ([_ [xh . xt] [xh . ot]]
   (all
     (rel xh)
     (filterg rel xt ot)))
  ; TODO ensure relation did not succeed!
  ([_ [xh . xt] ot] (filterg rel xt ot)))

(fact 
  (run 1 [q] (filterg emptyo [[:a] q [:b]] [[]])) => [[]])


; (defne my-everyg
;   [rel coll]
;   (reduceg 
; 
;   ))
