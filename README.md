# Rusp: Modern Clojure-Style Lisp Interpreter in Rust

A lightweight Lisp interpreter written in Rust featuring modern Clojure-inspired syntax, lexical closures, vector literals, typed runtime values, line/column diagnostic error reporting, and an easily extendable builtin system.

## Syntax & Features

- **Forms**: `defn`, `def`, `fn`, `let [v e]`, `if`, `do`, `quote` (`'`)
- **Data Types**: Integers (`42`), Floats (`3.14`), Booleans (`true`, `false`), `nil`, Strings (`"hello\nworld"`), Symbols (`foo`), Lists (`(1 2 3)`), Vectors (`[1 2 3]`)
- **Lexical Closures**: Functions capture their definition-time scope
- **Collections Operations**: `first`, `rest`, `second`, `third`, `nth`, `conj`, `push`, `reverse`, `count`
- **Error Diagnostics**: Span-aware error messages with line and column numbers
