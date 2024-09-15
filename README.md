# Simple Rust LISP Interpreter

This project is a simple LISP interpreter written in Rust, inspired by "Writing an Interpreter in Go."

It is purely for my own educational purposes.

## Lisp Spec
http://clhs.lisp.se/Front/index.htm

https://rextester.com/l/common_lisp_online_compiler 

## My goal
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

## Arithmetic & Comparison
- [x] `+`
- [x] `-`
- [x] `*`
- [x] `/`
- [x] `>`
- [x] `<`
- [x] `=`
- [x] `/=`
- [x] `not`
- [x] `mod`

## Built-ins
- [x] `let`
- [x] `print`
- [x] `if` 
```lisp
;; else is optional
(if condition then-expression else-expression) 
```
- [x] `cond` 
```lisp
(cond 
    (condition1 expression1) 
    (condition2 expression2) 
    (t default-expression))
```
- [x] `zerop` - `(zerop number)`
- [x] `defun`
```lisp
(defun function-name (parameters) body)
```
- [x] `dotimes`