(defun fizzbuzz (n)
  (dotimes (i n)
    (let ((num (+ i 1)))
      (cond
        ((and (zerop (mod num 3)) (zerop (mod num 5))) (print "FizzBuzz"))
        ((zerop (mod num 3)) (print "Fizz"))
        ((zerop (mod num 5)) (print "Buzz"))
        (T (print num))))))

(fizzbuzz 30)