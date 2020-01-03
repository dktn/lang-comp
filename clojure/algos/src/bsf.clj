(ns bsf)

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
    (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

(def graph1
  {"you"    '("alice" "bob" "claire")
   "bob"    '("anuj" "peggy")
   "alice"  '("peggy")
   "claire" '("thom" "johny")
   "anuj"   '()
   "peggy"  '()
   "thom"   '()
   "johny"  '()
  })

(defn is-seller [n]
  (= (subs n (dec (count n))) "m"))

(defn search [graph name]
  (letfn
    [(searchInQueue [searched queue]
      (let
        [person (peek queue)
         persons (pop queue)]
        (cond
          (contains? searched person) (recur searched persons)
          (is-seller person) person
          :else (recur (conj searched person) (into persons (graph person)))
        )))]
  (searchInQueue #{} (into (queue) (graph name)))))

(defn -main []
  (println (search graph1 "you"))
  (println "[done]"))
