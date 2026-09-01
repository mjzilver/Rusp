(defn fizzbuzz-item [num]
  (if (and (zero? (mod num 3)) (zero? (mod num 5)))
    "FizzBuzz"
    (if (zero? (mod num 3))
      "Fizz"
      (if (zero? (mod num 5))
        "Buzz"
        num))))

(defn build-fizzbuzz [current n acc]
  (if (> current n)
    acc
    (build-fizzbuzz (+ current 1) n (conj acc (fizzbuzz-item current)))))

(defn fizzbuzz [n]
  (build-fizzbuzz 1 n []))

(print (fizzbuzz 30))
