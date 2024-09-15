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
        ((and (zerop (mod num 3)) (zerop (mod num 5))) (format t "FizzBuzz~%"))
        ((zerop (mod num 3)) (format t "Fizz~%"))
        ((zerop (mod num 5)) (format t "Buzz~%"))
        (T (format t "~a~%" num))))))

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
- [ ] `mod`
- [ ] `rem`

## Built-ins
- [x] `let`
- [x] `print`
- [ ] `len`
- [x] `if` 
```lisp
;; else is optional
(if condition then-expression else-expression) 
```
- [ ] `loop`
- [ ] `cond` 
```lisp
(cond 
    (condition1 expression1) 
    (condition2 expression2) 
    (t default-expression))
```
- [ ] `zerop` - `(zerop number)`
- [ ] `defun`
```lisp
(defun function-name (parameters) body)
```
- [ ] `lists`
- [ ] `dotimes`
- [ ] `format`
```lisp
(format destination control-string &rest args)`
```