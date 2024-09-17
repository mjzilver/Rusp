; ===========================
; NESTED CONDITIONALS
; ===========================
; Check if a number is between 1 and 10
(defun check-range (n)
  (cond
    ((< n 1) "below 1")
    ((> n 10) "above 10")
    (T "within range")))

(print (check-range 0))   ; Should print below 1
(print (check-range 5))   ; Should print within range
(print (check-range 15))  ; Should print abovd 10
