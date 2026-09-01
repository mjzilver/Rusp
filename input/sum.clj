(defn sum-numbers [n]
  (if (= n 0)
    0
    (+ n (sum-numbers (- n 1)))))

(print (sum-numbers 50))