# This file defines the static semantics of Asteria, that is to say, the type system. It is defined as a bidirectional type checker, with simple unification for implicit arguments and types.

# The typechecker uses a simple mechanism for handling implicit instantiation of foralls when they're used in non-forall elims. The term in scrutinee position must synthesize a type, which is then instantiated as much as possible until a non-forall type constructor is reached, and then the remainder of the premises are checked, and then the resulting unifications are substituted, and the remaining un-unified variables are abstracted in reverse order to how they were abstracted.

# During typechecking, the term in the surface language is elaborated into a term in the core language, which is fully explicit form that has no implicit arguments anywhere and which lacks various user-friendly conveniences.

class Elaborator(object):

    def check(context: Context, type: Surface.Type, term: Surface.Term) -> Pair[Core.Term, Core.Type]:
        pass

    def synth(context: Context, term: Surface.Term) -> Pair[Core.Term, Core.Type]:
        pass
