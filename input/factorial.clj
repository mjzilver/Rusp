(defn fact [n]
  (if (zero? n)
    1
    (* n (fact (- n 1)))))

(print (fact 5))