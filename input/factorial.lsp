(defun fact (n)
  (if (zerop n)
      1
      (* n (fact (- n 1)))))

(print (fact 5))