# Simple Rust LISP Interpreter

This project is a simple LISP interpreter written in Rust, inspired by "Writing an Interpreter in Go."

It is purely for my own educational purposes.

## Lisp Spec
http://clhs.lisp.se/Front/index.htm

https://rextester.com/l/common_lisp_online_compiler 

## My goals

### Goal 1: Done
```lisp
(defun fizzbuzz (n)
  (dotimes (i n)
    (let ((num (+ i 1)))
      (cond
        ((and (zerop (mod num 3)) (zerop (mod num 5))) (print "FizzBuzz"))
        ((zerop (mod num 3)) (print "Fizz"))
        ((zerop (mod num 5)) (print "Buzz"))
        (T (print num))))))

(fizzbuzz 30)
```
### Goal 2
```lisp
(defun fact (n)
  (if (zerop n)
      1
      (* n (fact (- n 1)))))

(fact 5)
```