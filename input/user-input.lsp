(defun ask-for-input ()
  (print "Please enter some input: ")
  (let ((user-input "1"))
    (print (+ "You entered:" user-input))))

(print (ask-for-input))