(ns word-chains
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic.arithmetic :as arithmetic]
            [clojure.core.logic.fd :as fd])
  (:use [clojure.core.logic :rename {fact logic-fact facts logic-facts}]
        utils
        midje.sweet))

(defne one-letter-diffo
       [a b]
       ; Drop a letter
        ([[_ . at] _] (== at b))
        ([_ [_ . bt]] (== a bt))
       ;; Add a letter
        ([_ _]
         (fresh [ah]
           (conso ah a b)))
        ([_ _]
         (fresh [bh]
           (conso bh b a)))
       ; Change a letter
       ([[ah . at] [bh . bt]]
        (!= ah bh)
        (== at bt))
       ; Try further down the line
       ([[ah . at] [bh . bt]] (== ah bh) (one-letter-diffo at bt)))

; TODO!  Core.logic destructuring don't work on strings?  But they are sequency!
; Porque!
(run* [q]
     (one-letter-diffo (seq "aaa")  q))

(defn one-letter-diff?
  [a b]
  (>=
    (count 
      (run 1 [q]
           (one-letter-diffo (seq a) (seq b))))
    1))

(= true
   (and
     (= false (one-letter-diff? "aaa" "bbb"))
     (= false (one-letter-diff? "aaa" "aaabb"))
     (= false (one-letter-diff? "aaa" "aaa"))
     (= true (one-letter-diff? "aaa" "aac"))
     (= true (one-letter-diff? "aaa" "caa"))
     (= true (one-letter-diff? "aaa" "aca"))
     (= true (one-letter-diff? "aaa" "aaab"))
     (= true (one-letter-diff? "aaa" "baaa"))
     (= true (one-letter-diff? "aaa" "abaa"))
     (= true (one-letter-diff? "abc" "bc"))
     (= true (one-letter-diff? "abc" "ac"))
     (= true (one-letter-diff? "abc" "ab"))))

(defn unify-second-and-third-arg
  [rel]
  (fn [a b c]
    (all
      (== b c)
      (rel a b))))

(defn word-chaino [xs o]
  (fresh [reduceo]
      (permuteo xs o)
      (reduceg (unify-second-and-third-arg one-letter-diffo) o reduceo)))

(defn word-chain? [xs]
  (not
      (empty? 
        (let [res (run 1 [q]
                  (word-chaino (map seq xs) q))]
          (println res)
          res))))

(= true (word-chain? #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}))
(= false (word-chain? #{"cot" "hot" "bat" "fat"}))
(= false (word-chain? #{"to" "top" "stop" "tops" "toss"}))
(= true (word-chain? #{"spout" "do" "pot" "pout" "spot" "dot"}))
(= true (word-chain? #{"share" "hares" "shares" "hare" "are"}))
(= false (word-chain? #{"share" "hares" "hare" "are"}))
