# Simple Language Interpreter (SLI)
A simple typed-checked, functional language written solely in Racket

## Features
- **Arithmetic operations**: `+`, `-`, `*`, `/`
- **Boolean operations**: `<=`, `num-eq?`, `str-eq?`
- **Conditionals**: `if` expressions
- **Bindings**: `bind`, `recbind`
- **Anonymous Functions and Closures**: Define lambda functions with types and multiple arguments
- **Recursive functions**: Support for recursive function calls via `recbind`
- **Type Checking**: Type inference and checking for all expressions and operations
- **REPL or File Execution**: Execute SLI code interactively or from a file

## Usage
You can use the interpreter in two modes:

### 1. **Interactive REPL**
- Start the REPL:
```bash
racket sli.rkt
```
- Once in the REPL you can now input expressions:
```
sli> {+ 1 2}
3
sli> {if {<= 3 5} true false}
true
```
- Type `exit` to quit the REPL

### 2. **File Execution**
- Run a SLI program from a file:
```bash
racket sli.rkt program.sl
```
- This will read the file, process each line, and output the result of the final expression

## Syntax

### Expressions

- **Numbers**: `5`, `42`, etc.
- **Identifiers**: Variables or function names.
- **Strings**: `"hello"`, `"world"`, etc.
- **Conditionals**: `{ if <expr> <expr> <expr> }`
- **Bindings**: `{ bind [<id> : <type> = <expr>]* ... <expr> }`
- **Recursive Bindings**: `{ recbind [<id> : <type> = <expr>] <expr> }`
- **Function Definitions**: `{ ([<id> : <type>]*) => <expr> }`
- **Function Application**: `{ <expr> <expr> ... }`

### Types

- `num` (for numbers)
- `bool` (for booleans)
- `str` (for strings)
- Function types: `{ <type>* -> <type> }`

### Primitives

- **Arithmetic**:
  - `{+ <a> <b>}`: Sum of `a` and `b` (num).
  - `{- <a> <b>}`: Difference of `a` and `b` (num).
  - ` {* <a> <b>}`: Product of `a` and `b` (num).
  - `{ / <a> <b>}`: Division of `a` by `b` (num), with error if `b` is 0.
  
- **Comparison**:
  - `{<= <a> <b>}`: True if `a` is less than or equal to `b` (bool).
  - `{num-eq? <a> <b>}`: True if `a` equals `b` (bool).
  - `{str-eq? <a> <b>}`: True if strings `a` and `b` are equal (bool).

- **Boolean Values**:
  - `true`: Boolean literal `true`.
  - `false`: Boolean literal `false`.

## Example
Calculating factorials using recursion:
```
{recbind [factorial
          : {num -> num}
          = {([n : num]) => {if {<= n 0} 1 {* n {factorial {- n 1}}}}}]
 {factorial 5}}
```

## Future Improvements
- Support for more built-in functions and operations.
- Improve the type checker to handle more complex type scenarios.
- Add more detailed error reporting for invalid programs
- Add support for complex data types like arrays
