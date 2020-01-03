(ns fizz-buzz)

(require '[clojure.core.match :refer [match]])

(doseq [n (range 1 101)]
  (println
    (match [(mod n 3) (mod n 5)]
      [0 0] "FizzBuzz"
      [0 _] "Fizz"
      [_ 0] "Buzz"
      :else n)))


(defn -main []
  (println "[done]"))
