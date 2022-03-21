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


@dataclass
class TypeKind(Kind):
    pass


@dataclass
class ArrowKind(Kind):
    argument_kind: Kind
    return_kind: Kind


################  Types  ################

@dataclass
class TyVarKinding(Core):
    tyvar: str
    kind: Kind


@dataclass
class Type(Core):
    pass


@dataclass
class VariableType(Type):
    name: str


@dataclass
class NameType(Type):
    modules: List[str]
    name : str
    arguments: List[Type]


@dataclass
class FunctionType(Type):
    argument_type: Type
    return_type: Type


@dataclass
class ForallType(Type):
    tyvar_kinding: TyVarKinding
    scope: Type


@dataclass
class LambdaType(Type):
    tyvar_kinding: TyVarKinding
    body: Type

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


@dataclass
class ConstructorDeclaration(Core):
    name: str
    type_parameters: List[ConstructorTypeParameter]
    parameters: List[ConstructorParameter]
    return_type: Type


@dataclass
class DataDeclaration(Declaration):
    name: str
    tyvar_kindings: List[TyVarKinding]
    constructor_declarations: List[ConstructorDeclaration]


################  Type Signature  ################

@dataclass
class TypeSignature(Declaration):
    name: str
    type: Type



################  Patterns  ################

@dataclass
class Pattern(Core):
    pass


@dataclass
class VariablePattern(Pattern):
    name: str


@dataclass
class ConstructorPattern(Pattern):
    constructor: str
    arguments: List[Pattern]


################  Terms  ################

@dataclass
class Term(Core):
    pass


@dataclass
class TypeAnnotationTerm(Term):
    term: Term
    type: Type


@dataclass
class VariableTerm(Term):
    name: str


@dataclass
class DeclaredTermNameTerm(Term):
    modules: List[str]
    name_parts: List[str]


@dataclass
class ConstructorTerm(Term):
    constructor: str
    arguments: List[Term]


@dataclass
class LambdaTerm(Term):
    formal_parameter: str
    formal_parameter_type: Type
    body: Term


@dataclass
class AbstractionTerm(Term):
    formal_type_parameter: str
    formal_type_parameter_kind: Kind
    body: Term


@dataclass
class ApplicationTerm(Term):
    function: Term
    argument: Term


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
    scrutinee: Term
    clauses: List[CaseClause]


################  Term Equation  ################


@dataclass
class TermEquation(Declaration):
    name: str
    definition: Term


################  Modules  ################


@dataclass
class Module(Core):
    name: str
    declarations: List[Declaration]
