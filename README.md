# Simple Rust LISP Interpreter

This project is a simple LISP interpreter written in Rust, inspired by "Writing an Interpreter in Go."

It is purely for my own educational purposes.

## Lisp Spec
http://clhs.lisp.se/Front/index.htm

https://rextester.com/l/common_lisp_online_compiler 

## My goals
### Goal 1 - DONE
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
### Goal 2 - DONE
```lisp
(defun fact (n)
  (if (zerop n)
      1
      (* n (fact (- n 1)))))

(fact 5)
```

### Goal 3
```lisp
(setq my-list '(1 2 3 4 5))
(first my-list)   ;; 1
(nth 2 my-list)   ;; 3
(push 0 my-list)  ;;  (0 1 2 3 4 5)
(setq my-list (append my-list '(6)))  ;;  (0 1 2 3 4 5 6)
(print my-list)
```

### Goal 4
```lisp
(defun ask-for-input ()
  (print "Please enter some input: ")
  (let ((user-input (read-line)))
    (print (+ "You entered:" user-input))))
```


### TODO
- [ ] `lists`
- [ ] `format`
- [ ] `read-line`
- [ ] `concatenate` (now + works for strings, it should not)
- [ ] `format`