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

## Built-ins
- [x] `+`
- [x] `-`
- [x] `*`
- [x] `/`
- [x] `>`
- [x] `<`
- [x] `=`
- [x] `not`
- [ ] `mod`
- [x] `let`
- [x] `print`
- [ ] `len`
- [ ] `if`
- [ ] `loop`
- [ ] `cond`
- [ ] `zerop` - Tests if a number is zero.
- [ ] `functions`
- [ ] `lists`
- [ ] `dotimes`
- [ ] `format`