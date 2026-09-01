(defn fizzbuzz-step [num]
  (if (and (zero? (mod num 3)) (zero? (mod num 5)))
    (print "FizzBuzz")
    (if (zero? (mod num 3))
      (print "Fizz")
      (if (zero? (mod num 5))
        (print "Buzz")
        (print num)))))

(defn fizzbuzz [n current]
  (if (<= current n)
    (do
      (fizzbuzz-step current)
      (fizzbuzz n (+ current 1)))
    nil))

(fizzbuzz 30 1)