(defun fizzbuzz (n)
  (let ((result '()))  ; Initialize an empty list
    (dotimes (i n)
      (let ((num (+ i 1)))
        (push
         (cond
           ((and (zerop (mod num 3)) (zerop (mod num 5))) "FizzBuzz")
           ((zerop (mod num 3)) "Fizz")
           ((zerop (mod num 5)) "Buzz")
           (t num))
         result)))  ; Push the result onto the list
    (reverse result)))  ; Reverse the list to maintain the correct order

(print (fizzbuzz 30))
