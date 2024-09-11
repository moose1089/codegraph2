(ns codegraph.other)


(defn fact
  [n]
  (if (<= n 1)
    1
    (* n (fact (dec n)))))

(fact 4)
