; ===========================
; SUM 
; ===========================
(defun sum-numbers (n)
  (if (= n 0)
      0
      (+ n (sum-numbers (- n 1)))))
; Rust' stack will overflow at ~78
(print (sum-numbers 50))