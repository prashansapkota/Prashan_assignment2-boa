# Week 2: Boa - Variables and Binary Operators

## Overview
In this assignment, you'll extend your compiler to support **Boa** (Binary Operators And variables), adding `let` bindings and binary arithmetic operators. This introduces the stack for variable storage and the need for an environment to track variable locations.

## Learning Objectives
- Implement variable binding and lookup
- Generate code that uses the stack
- Handle binary operations
- Understand environment/symbol table management
- Learn about register allocation basics

## Building on Week 1
You should start from your Adder compiler or use the provided starter code. The key new concepts are:
- **Variables**: Store values on the stack
- **Let bindings**: Create new scope with variables
- **Binary operators**: Operations on two values

## The Boa Language

### Concrete Syntax
```
<expr> :=
  | <number>
  | <identifier>
  | (let ((<identifier> <expr>)+) <expr>)
  | (add1 <expr>)
  | (sub1 <expr>)
  | (+ <expr> <expr>)
  | (- <expr> <expr>)
  | (* <expr> <expr>)

<identifier> := [a-zA-Z][a-zA-Z0-9_-]*  (but not reserved words)
```

Reserved words: `let`, `add1`, `sub1`

### Abstract Syntax (Rust)
The starter code provides the following AST definition:
```rust
enum Op1 { Add1, Sub1 }
enum Op2 { Plus, Minus, Times }

enum Expr {
    Number(i32),
    Id(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
}
```

### Semantics

**Let bindings**: Evaluate binding expressions left-to-right, storing each result before evaluating the next. Each binding is visible in subsequent bindings and the body.

```scheme
(let ((x 5)
      (y (+ x 1)))  ; x is visible here
  (* x y))          ; both x and y visible here
; Result: 30
```

**Variables**: Look up the most recent binding of the identifier.

**Binary operators**: Evaluate both operands (left-to-right), then apply the operation.

### Examples

| Program | Result |
|---------|--------|
| `(let ((x 5)) (+ x 1))` | 6 |
| `(let ((x 5) (y 10)) (+ x y))` | 15 |
| `(let ((x 10) (y (+ x 5))) (* x y))` | 150 |
| `(let ((a 1) (b 2)) (let ((c (+ a b))) (* c c)))` | 9 |

## Implementation Strategy

### The Stack
The x86-64 stack grows downward (toward lower addresses). Use negative offsets from `rsp` to store local variables:

```
        Higher addresses
        ---------------
rsp-16  | variable 1  |  (si=2, offset=-16)
        ---------------
rsp-24  | variable 2  |  (si=3, offset=-24)
        ---------------
rsp-32  | variable 3  |  (si=4, offset=-32)
        ---------------
        Lower addresses
```

### Environment
The starter code uses the `im` crate for immutable HashMaps. This makes it easy to extend the environment for nested scopes without mutating the original.

```rust
use im::HashMap;
// env.update(name, offset) returns a new HashMap with the binding added
```

### Stack Index (si)
- The `si` parameter tracks the next available stack slot
- Start at `si=2` (offset -16), reserving slot 1 for potential future use
- Each variable uses 8 bytes, so offset = `-8 * si`

## Implementation Tasks

The starter code in `src/main.rs` provides the structure. You need to implement:

### Task 1: `parse_expr` and `parse_bind`
Parse S-expressions into the `Expr` AST:
- Numbers: `Sexp::Atom(I(n))` → `Expr::Number(...)`
- Identifiers: `Sexp::Atom(S(name))` → `Expr::Id(...)`
- Operations: `Sexp::List(...)` → match on operator and recursively parse

**Hint**: Use pattern matching on `&vec[..]` to match list structure.

### Task 2: `compile_to_instrs`
Generate assembly instructions for each expression type:

- **Number**: Move immediate to RAX
- **Id**: Look up in environment, load from stack offset
- **UnOp**: Compile subexpression, then add/sub 1
- **BinOp**:
  1. Compile left operand
  2. Save result to stack at current `si`
  3. Compile right operand with `si+1`
  4. Perform operation between saved value and RAX
- **Let**:
  1. Check for duplicate bindings
  2. For each binding: compile expression, store at stack slot, update environment
  3. Compile body with extended environment

## Error Handling

Your compiler must detect and report these errors:

| Error | Message |
|-------|---------|
| Duplicate binding in same let | `panic!("Duplicate binding")` |
| Unbound variable | `panic!("Unbound variable identifier {name}")` |
| Invalid syntax | `panic!("Invalid")` |

## Testing

Test files are provided in the `test/` directory:

| File | Program | Expected |
|------|---------|----------|
| simple.snek | `42` | 42 |
| add.snek | `(add1 (add1 (add1 3)))` | 6 |
| binop.snek | `(+ (* 2 3) 3)` | 9 |
| let_simple.snek | `(let ((x 5)) (+ x x))` | 10 |
| let_multi.snek | `(let ((x 5) (y 6)) (+ x y))` | 11 |
| nested.snek | nested let expression | 25 |

Run tests with:
```bash
make test        # Build and run all test programs
cargo test       # Run unit tests
```

## Building

```bash
cargo build      # Build the compiler
cargo run -- test/simple.snek test/simple.s   # Compile a program
make test/simple.run   # Build and link executable
./test/simple.run      # Run the compiled program
```

## Grading Rubric

- **Parser (25%)**: Correctly parses all valid Boa programs
- **Variable Lookup (20%)**: Correct environment management
- **Stack Management (20%)**: Proper stack offset calculation
- **Binary Operations (20%)**: Correct code for all operators
- **Error Handling (15%)**: Proper error messages for invalid programs

## Common Pitfalls

1. **Stack Offsets**: Remember offsets are negative from RSP
2. **Operator Order**: For `(- a b)`, result should be `a - b`, not `b - a`
3. **Environment Shadowing**: Inner bindings should shadow outer ones (this is allowed)
4. **Duplicate Bindings**: Same name twice in ONE let is an error; shadowing across lets is fine
5. **Binary Op Temp Storage**: Must save left operand before evaluating right

## Resources

- [x86-64 Instruction Reference](https://www.felixcloutier.com/x86/)
- [im crate documentation](https://docs.rs/im/latest/im/)
- Course lecture notes on stack management

## Deliverables

Submit your completed `src/main.rs` with:
1. Working `parse_expr` and `parse_bind` functions
2. Working `compile_to_instrs` function
3. All provided tests passing
4. (Optional) Additional test cases you created

Good luck! This assignment significantly expands your compiler's capabilities.
