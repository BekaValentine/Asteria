# YAPL: Yet Another Programming Language

A pure, strict, eager, Haskell-like functional programming language for both scripting and systems programming, and maybe more!

# Features

- Pure: All side effects except non-termination are represented in the type system
- Strict: Has a strict denotational semantics (omega-CPOs)
- Eager: Variables are bound to values not thunks (contra Haskell)
- Strongly Typed: Higher-kinded System FC with no type inference and type classes
- Coverage Checking: all case expressions and pattern matching must be total
- Type Classes: Haskell-style type classes are used for ad hoc polymorphism
- Typed Holes: Parts of the program can be left unspecified and the type checker can provide information about it and its context
- Modules: A clean module system like Agda's, usable for doing separate compilation if necessary.
- Built-in Static Compilation: Out-of-the-box static compilation programs into a single binary with no dynamically linked libraries
