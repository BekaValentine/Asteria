# YAPL: Yet Another Programming Language

A pure, strict, eager, Haskell-like functional programming language for both scripting and systems programming, and maybe more!

# Language Itself

- Pure: All side effects except non-termination are represented in the type system
- Strict: Has a strict denotational semantics (omega-CPOs)
- Eager: Variables are bound to values not thunks (contra Haskell)
- Strongly Typed: Higher-kinded System FC with no type inference and type classes
- Coverage Checking: all case expressions and pattern matching must be total
- Type Classes: Haskell-style type classes are used for ad hoc polymorphism
- Typed Holes: Parts of the program can be left unspecified and the type checker can provide information about it and its context
- Modules: A clean module system like Agda's, usable for doing separate compilation if necessary.
- FFI for a variety of ABIs
- Efficient state types (scoped/bounded)
- Equational rewrite rules
- Non-blocking IO or equivalently performant construct
- Shared memory or equivalently performant construct
- Multicore support, multi-processor support

# Compiler

- Built-in Static Compilation: Out-of-the-box static compilation programs into a single binary with no dynamically linked libraries
- Good error messages (ala Elm)

# Ecosystem

- Good package management and versioning
- Testing (beyond Quickcheck-style "just a program" testing)
- A good standard library (core datatypes + operations + efficient and elegant binary data handling)
- A good set of essential utilities (net-y things, crypto)
- Debugger
