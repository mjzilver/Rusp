# Simple Rust LISP Interpreter

This project is a simple LISP interpreter written in Rust, inspired by "Writing an Interpreter in Go."

It is purely for my own educational purposes.

## Lisp Spec
http://clhs.lisp.se/Front/index.htm

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

## Features
- [ ] `if`
- [ ] `loop`
- [ ] `functions`
- [ ] `lists`
- [ ] `macro`

## Notes on LISP
Inspiration: https://www.youtube.com/watch?v=V02SQDh47gA

```
(function arguments...)

(+ 1 2)
 ^ ^ ^
 | | Argument 2 
 | Argument 1
 Function

+ is essentially a function that takes 2 arguments
could be represented as
+(a, b) = {
    return a + b
}
```