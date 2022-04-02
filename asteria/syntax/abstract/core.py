# This file defines the core language for Asteria. This is very similar to the Core language but it lacks all of the conveniences and makes all things explicit and uniquely representable.

from dataclasses import dataclass
from lark import Tree
from typing import List, Optional, TypeVar


@dataclass
class Core(object):
    pass

################  Kinds  ################


@dataclass
class Kind(Core):
    pass


# Type
# TypeKind()
@dataclass
class TypeKind(Kind):
    pass


# Type -> Type
# ArrowKind(TypeKind(), TypeKind())
@dataclass
class ArrowKind(Kind):
    argument_kind: Kind
    return_kind: Kind


################  Names  ################

# Data.List$List
# DeclaredTypeConstructorName(["Data","List"], "List")
@dataclass
class DeclaredTypeConstructorName(Core):
    modules: List[str]
    name: str


# Data.List$List$Cons
# DeclaredConstructorName(DeclaredTypeConstructorName(["Data","List"], "List"), "Cons")
@dataclass
class DeclaredConstructorName(Core):
    type_name: DeclaredTypeConstructorName
    name: str


# Data.List$reverse$reverseOnto
# DeclaredTermName(["Data", "List"], "reverse", ["reverseOnto"])
@dataclass
class DeclaredTermName(Core):
    modules: List[str]
    root_name: str
    sub_names: List[str]

################  Types  ################


# a : Type
# TyVarkinding("a", TypeKind())
@dataclass
class TyVarKinding(Core):
    tyvar: str
    kind: Kind


@dataclass
class Type(Core):
    pass


# a
# VariableType("a")
@dataclass
class VariableType(Type):
    name: str


# Data.Pair$Pair(a;b)
# ConstructorType("Pair", [VariableType("a"), VariableType("b")])
@dataclass
class ConstructorType(Type):
    name: DeclaredTypeConstructorName
    arguments: List[Type]


# a -> b
# FunctionType(VariableType("a"), VariableType("b"))
@dataclass
class FunctionType(Type):
    argument_type: Type
    return_type: Type


# forall (a : Type). a
# ForallType(TyVarKinding("a", TypeKind()), VariableType("a"))
@dataclass
class ForallType(Type):
    tyvar_kinding: TyVarKinding
    scope: Type


# \(a : Type) -> a
# LambdaType(TyVarKinding("a", TypeKind()), VariableType("a"))
@dataclass
class LambdaType(Type):
    tyvar_kinding: TyVarKinding
    body: Type

# f $ a
# ApplicationType(VariableType("f"), VariableType("a"))


@dataclass
class ApplicationType(Type):
    function: Type
    argument: Type


################  Declarations  ################

@dataclass
class Declaration(Core):
    pass


################  Data Declarations  ################


@dataclass
class ConstructorTypeParameter(Core):
    tyvar: str
    kind: Kind


@dataclass
class ConstructorParameter(Core):
    var: str
    type: Type


# constructor Data.Pair$Pair$MkPair {a : Type} {b : Type} (x : a) (y : b) : Data.Pair$Pair(a;b);;
@dataclass
class ConstructorDeclaration(Declaration):
    name: DeclaredConstructorName
    type_parameters: List[ConstructorTypeParameter]
    parameters: List[ConstructorParameter]
    return_type_name: DeclaredTypeConstructorName
    return_type_params: List[Type]


# datatype Data.Pair$Pair (a : Type) (b : Type);;
@dataclass
class TypeConstructorDeclaration(Declaration):
    name: DeclaredTypeConstructorName
    tyvar_kindings: List[TyVarKinding]


################  Type Signature  ################

# Data.List$reverse : ...;;
@dataclass
class TypeSignature(Declaration):
    name: DeclaredTermName
    type: Type


################  Patterns  ################


@dataclass
class Pattern(Core):
    pass


# x
# VariablePattern("x")
@dataclass
class VariablePattern(Pattern):
    name: str


# Cons(x; xs)
# ConstructorPattern("Cons", [VariablePattern("x", VariablePattern("xs")])
@dataclass
class ConstructorPattern(Pattern):
    constructor: DeclaredConstructorName
    arguments: List[Pattern]


################  Terms  ################

@dataclass
class Term(Core):
    pass


# x : a
# AnnotationTerm(VariableTerm("x"), VariableType("a"))
@dataclass
class TypeAnnotationTerm(Term):
    term: Term
    type: Type


# x
# VariableTerm("x")
@dataclass
class VariableTerm(Term):
    name: str


@dataclass
class DeclaredTermNameTerm(Term):
    name: DeclaredTermName


# Pair(x;y)
# ConstructorTerm("Pair", [VariableTerm("x"), VariableTerm("y")])
@dataclass
class ConstructorTerm(Term):
    constructor: DeclaredConstructorName
    arguments: List[Term]


# \(x : a) -> x
# LambdaTerm("x", VariableType("a"), VariableTerm("x"))
@dataclass
class LambdaTerm(Term):
    formal_parameter: str
    formal_parameter_type: Type
    body: Term


#\{a : Type} -> x
# AbstractionTerm("a", TypeKind(), VariableTerm("x"))
@dataclass
class AbstractionTerm(Term):
    formal_type_parameter: str
    formal_type_parameter_kind: Kind
    body: Term


# f x
# ApplicationTerm(VariableTerm("f"), VariableTerm("x"))
@dataclass
class ApplicationTerm(Term):
    function: Term
    argument: Term


# f {a}
# InstantiationTerm(VariableTerm("f"), VariableType("a"))
@dataclass
class InstantiationTerm(Term):
    function: Term
    argument: Type


@dataclass
class Guard(Core):
    term: Optional[Term]


@dataclass
class CaseClause(Core):
    pattern: List[Pattern]
    guard: Guard
    body: Term


@dataclass
class CaseTerm(Term):
    scrutinee: List[Term]
    clauses: List[CaseClause]


################  Term Equation  ################


# Data.List$reverse$reverseOnto = ...;;
@dataclass
class TermEquation(Declaration):
    name: DeclaredTermName
    definition: Term


################  Modules  ################


@dataclass
class Module(Core):
    name: str
    declarations: List[Declaration]
