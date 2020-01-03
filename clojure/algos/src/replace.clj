(ns replace)
  ; (:require [clojure.core.match :as t]))

(require '[clojure.core.match :refer [match]])

(defn replaceFirstWith4 [y z u] ; FIXME
  (match u
    [] '()
    [a] '(a)
    [a b] '(a b)
    _ '(a b)
    ; [a & r] :when (= z a) '(a)
  ))

(defn replaceFirstWith [y z u]
  (match [(first u) (rest u)]
    [nil _] '()
    [x :guard #(= % y) xs] (conj xs z)
    [x xs] (conj (replaceFirstWith y z xs) x)
  ))

(defn replaceFirstWith2 [y z [h & more :as u]]
  (cond
    (empty? u) '()
    (= h y) (conj more z)
    :else (conj (replaceFirstWith2 y z more) h)
  ))

(defn replaceFirstWith3 [y z u]
  (letfn
    [(replaceAcc [acc done [h & more :as u]]
      (cond
        (empty? u) (reverse acc)
        (true? done) (into u acc)
        (= h y) (recur (conj acc z) true more)
        :else (recur (conj acc h) false more)))]
    (replaceAcc '() false u)))

(defn replaceLastWith [y z u]
  (reverse (replaceFirstWith y z (reverse u)))
)

(defn len [c u]
  (match [(first u) (rest u)]
    [nil _] c
    [x xs] (recur (+ c 1) xs)))

(defn len2 [c [h & more :as s]]
  (if (seq s)
    (recur (+ c 1) more)
    c))

(defn dosth []
  (let [x '(1 2)]
    (match [x]
          [([1] :seq)] :a0
          [([1 & r] :seq)] [:a1 r]
          :else nil))
)

(defn -main []
  (println "first")
  (println (replaceFirstWith  2 5 '(1 2 3 2 1)))
  (println (replaceFirstWith2 2 5 '(1 2 3 2 1)))
  (println (replaceFirstWith3 2 5 '(1 2 3 2 1)))
  (println "last")
  (println (replaceLastWith   2 5 '(1 2 3 2 1)))
  (println "len")
  (println (len  0 '(1 2 3 4 5)))
  (println (len2 0 '(1 2 3 4 5)))
)
