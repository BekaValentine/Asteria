- Coverage Checking
  - ```
    data Foo [a : Type] where
       | MkBool (b : Type$Bool()) : Type$Foo(Type$Bool())
       | MkInt (i : Type$Int()) : Type$Foo(Type$Int())
       | MkAny {t : Type} (x : t) : Type$Foo(t)
       ;;

    f : forall {a : Type}. Type$Foo(a) -> a
      = \{a} -> \x ->
          case x of
             | MkBool(;b) -> b
             | MkInt(;i)  -> i
             ;;
    ```
  - coverage checking algo
    ```
    a : Type !- MkBool(;b), MkInt(;i) covers Type$Foo(a)
      !- MkBool(;b) covers Type$Foo(Type$Bool())
      !- MkInt(;i) covers Type$Foo(Type$Int())
    ```
- reorg error tests into subdirs ?
- empty data and case syntaxes
  - case M of | #  ?????
    - use something like agda's absurd patterns?

- ensure that error messages are maximally informative about their source/cause
  - checking pushes information in but the source of that information matters for explaining the error
