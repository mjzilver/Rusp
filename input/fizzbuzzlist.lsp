(defun fizzbuzz (n)
  (let ((result '()))
    (dotimes (i n)
      (let ((num (+ i 1)))
        (push
         (cond
           ((and (zerop (mod num 3)) (zerop (mod num 5))) "FizzBuzz")
           ((zerop (mod num 3)) "Fizz")
           ((zerop (mod num 5)) "Buzz")
           (T num))
         result)))
    (reverse result)))

(print (fizzbuzz 30))
