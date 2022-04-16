# This file defines the core language for Asteria. This is very similar to the Core language but it lacks all of the conveniences and makes all things explicit and uniquely representable.

from dataclasses import dataclass
from lark import Tree, Token
from typing import List, Optional, Tuple

from asteria.utils import *


@dataclass
class Core(Syntax):
    pass

################  Kinds  ################


@dataclass
class Kind(Core):

    def pretty(self, prec=None) -> str:
        return f'(unknown kind: {self})'


def Kind_from_cst(cst: Tree) -> Kind:

    if cst.data == 'type_kind':
        return TypeKind()

    elif cst.data == 'function_kind':
        return FunctionKind(
            argument_kind=Kind_from_cst(cst.children[0]),
            return_kind=Kind_from_cst(cst.children[1]))

    raise

# Type
# TypeKind()


@dataclass
class TypeKind(Kind):

    def pretty(self, prec=None) -> str:
        return 'Type'


# Type -> Type
# FunctionKind(TypeKind(), TypeKind())
@dataclass
class FunctionKind(Kind):
    argument_kind: Kind
    return_kind: Kind

    def pretty(self, prec=None) -> str:
        p = f'{self.argument_kind.pretty()} -> {self.return_kind.pretty()}'
        if prec is None:
            return p
        elif prec == 'argument':
            return f'({p})'

        raise


################  Names  ################

def ModuleName_from_cst(cst: Tree) -> List[str]:
    if not isinstance(cst, Token):
        raise
    return [tok.value for tok in cst.children]

# Data.List$List
# DeclaredTypeConstructorName(["Data","List"], "List")


@dataclass
class DeclaredTypeConstructorName(object):
    modules: List[str]
    name: str

    def __hash__(self) -> int:
        return self.pretty().__hash__()

    def __eq__(self, other) -> bool:
        return isinstance(other, DeclaredTypeConstructorName) and self.__hash__() == other.__hash__()

    def pretty(self) -> str:
        return '.'.join(self.modules) + '$' + self.name


def DeclaredTypeConstructorName_from_cst(cst: Tree) -> DeclaredTypeConstructorName:
    return DeclaredTypeConstructorName(
        modules=ModuleName_from_cst(cst.children[0]),
        name=cst.children[1].value)


# Data.List$List$Cons
# DeclaredConstructorName(DeclaredTypeConstructorName(["Data","List"], "List"), "Cons")
@dataclass
class DeclaredConstructorName(object):
    type_name: DeclaredTypeConstructorName
    name: str

    def __hash__(self) -> int:
        return self.pretty().__hash__()

    def __eq__(self, other) -> bool:
        return isinstance(other, DeclaredConstructorName) and self.__hash__() == other.__hash__()

    def pretty(self) -> str:
        return self.type_name.pretty() + '$' + self.name


def DeclaredConstructorName_from_cst(cst: Tree) -> DeclaredConstructorName:
    if isinstance(cst, Token):
        raise
    return DeclaredConstructorName(
        type_name=DeclaredTypeConstructorName_from_cst(cst.children[0]),
        name=cst.children[1].value)


# reverse$reverseOnto
# TermNameWithSubnames("reverse", ["reverseOnto"])
@dataclass
class TermNameWithSubnames(object):
    root_name: str
    sub_names: List[str]

    def __hash__(self) -> int:
        return self.pretty().__hash__()

    def __eq__(self, other) -> bool:
        return isinstance(other, TermNameWithSubnames) and self.__hash__() == other.__hash__()

    def pretty(self) -> str:
        return self.root_name + ''.join(['$' + n for n in self.sub_names])


def TermNameWithSubnames_from_cst(cst: Tree) -> TermNameWithSubnames:
    return TermNameWithSubnames(
        root_name=cst.children[0].value,
        sub_names=[name.value for name in cst.children[1].children])


# Data.List$reverse$reverseOnto
# DeclaredTermName(["Data", "List"], "reverse", ["reverseOnto"])
@dataclass
class DeclaredTermName(object):
    modules: List[str]
    name: TermNameWithSubnames

    def __hash__(self) -> int:
        return self.pretty().__hash__()

    def __eq__(self, other) -> bool:
        return isinstance(other, DeclaredTermName) and self.__hash__() == other.__hash__()

    def pretty(self) -> str:
        return '.'.join(self.modules) + '$' + self.name.pretty()


def DeclaredTermName_from_cst(cst: Tree) -> DeclaredTermName:
    return DeclaredTermName(
        modules=ModuleName_from_cst(cst.children[0]),
        name=TermNameWithSubnames_from_cst(cst.children[1]))

################  Types  ################


# a : Type
# TyVarkinding("a", TypeKind())
@dataclass
class TyVarKinding(Core):
    tyvar: str
    kind: Kind


def TyVarKinding_from_cst(cst: Tree) -> TyVarKinding:
    return TyVarKinding(
        tyvar=cst.children[0].value,
        kind=Kind_from_cst(cst.children[1]))


@dataclass
class Type(Core):
    pass


def Type_from_cst(cst: Tree) -> Type:

    if cst.data == 'variable_type':
        return VariableType(cst.children[0].value)

    elif cst.data == 'constructor_type':
        return ConstructorType(
            name=DeclaredTypeConstructorName_from_cst(cst.children[0]),
            arguments=[Type_from_cst(arg) for arg in cst.children[1].children])

    elif cst.data == 'function_type':
        return FunctionType(
            argument_type=Type_from_cst(cst.children[0]),
            return_type=Type_from_cst(cst.children[1]))

    elif cst.data == 'forall_type':
        tvk = TyVarKinding_from_cst(cst.children[0])
        return ForallType(
            tyvar_kind=tvk.kind,
            scope=Scope(
                names=[tvk.tyvar],
                body=Type_from_cst(cst.children[1])))

    elif cst.data == 'lambda_type':
        tvk = TyVarKinding_from_cst(cst.children[0])
        return LambdaType(
            tyvar_kind=tvk.kind,
            body=Scope(
                names=[tvk.tyvar],
                body=Type_from_cst(cst.children[1])))

    elif cst.data == 'application_type':
        return ApplicationType(
            function=Type_from_cst(cst.children[0]),
            argument=Type_from_cst(cst.children[1]))

    elif cst.data == 'paren_type':
        return Type_from_cst(cst.children[0])

    raise


# x
# VariableType("")
@dataclass
class VariableType(Type):
    name: str

    def pretty(self, prec=None) -> str:
        return f'{self.name}'

    def rename(self, renaming):
        if renaming == {}:
            return self
        if self.name in renaming:
            return VariableType(renaming[self.name])
        else:
            return self


# Data.Pair$Pair(a;b)
# ConstructorType("Pair", [VariableType("a"), VariableType("b")])
@dataclass
class ConstructorType(Type):
    name: DeclaredTypeConstructorName
    arguments: List[Type]

    def pretty(self, prec=None) -> str:
        return self.name.pretty() + '(' + ','.join([t.pretty() for t in self.arguments]) + ')'


# a -> b
# FunctionType(VariableType("a"), VariableType("b"))
@dataclass
class FunctionType(Type):
    argument_type: Type
    return_type: Type

    def pretty(self, prec=None) -> str:
        p = f'{self.argument_type.pretty()} -> {self.return_type.pretty()}'
        if prec == 'argument':
            return f'({p})'
        else:
            return p


# forall (a : Type). a
# ForallType(TypeKind(), Scope(["a"], VariableType("a")))
@dataclass
class ForallType(Type):
    tyvar_kind: Kind
    scope: Scope

    def pretty(self, prec=None) -> str:
        p = f'forall ({self.scope.names[0]} : {self.tyvar_kind.pretty()}). {self.scope.body.pretty()}'
        if prec == 'argument':
            return f'({p})'
        else:
            return p


# \(a : Type) -> a
# LambdaType(TypeKind(), Scope(["a"], VariableType("a")))
@dataclass
class LambdaType(Type):
    tyvar_kind: Kind
    body: Scope

    def pretty(self, prec=None) -> str:
        p = f'\({self.body.names[0]} : {self.tyvar_kind.pretty()}) -> {self.body.body.pretty()}'
        if prec in ['argument', 'function', 'argument']:
            return f'({p})'
        else:
            return p


# f $ a
# ApplicationType(VariableType("f"), VariableType("a"))
@dataclass
class ApplicationType(Type):
    function: Type
    argument: Type

    def pretty(self, prec=None) -> str:
        p = f'{self.function.pretty()} {self.argument.pretty()}'
        if prec == 'argument':
            return f'({p})'
        else:
            return p


################  Declarations  ################

@dataclass
class Declaration(Core):
    pass


def Declaration_from_cst(cst: Tree) -> Declaration:
    if cst.data == 'data_declaration':
        return DataDeclaration(
            name=cst.children[0].value,
            signature=TypeConstructorSignature(
                parameters=[TyVarKinding_from_cst(
                    tvk) for tvk in cst.children[1].children]),
            constructors=[ConstructorDeclaration_from_cst(condecl) for condecl in cst.children[2].children])
    elif cst.data == 'term_declaration':
        return TermDeclaration(
            name=TermNameWithSubnames_from_cst(cst.children[0]),
            type=Type_from_cst(cst.children[1]),
            definition=Term_from_cst(cst.children[2]))

    raise


################  Data Declarations  ################


@dataclass
class ConstructorDeclTypeArgument(Core):
    tyvar: str
    kind: Kind


def ConstructorDeclTypeArgument_from_cst(cst: Tree) -> ConstructorDeclTypeArgument:
    return ConstructorDeclTypeArgument(
        tyvar=cst.children[0].value,
        kind=Kind_from_cst(cst.children[1]))


@dataclass
class ConstructorDeclArgument(Core):
    var: str
    type: Type


def ConstructorDeclArgument_from_cst(cst: Tree) -> ConstructorDeclArgument:
    return ConstructorDeclArgument(
        var=cst.children[0].value,
        type=Type_from_cst(cst.children[1]))


@dataclass
class ConstructorSignature(Core):
    type_parameters: List[Tuple[str, Kind]]
    parameters: List[Tuple[str, Type]]
    return_type: Type

    def pretty(self, prec=None) -> str:
        return ' '.join([f'{{{v} : {k.pretty()}}}' for v, k in self.type_parameters]) +\
               ' ' +\
               ' '.join([f'({v} : {k.pretty()})' for v, k in self.parameters]) +\
               ' ~> ' +\
               self.return_type.pretty()


# constructor Data.Pair$Pair$MkPair {a : Type} {b : Type} (x : a) (y : b) : Data.Pair$Pair(a;b);;
@dataclass
class ConstructorDeclaration(Core):
    name: str
    type_parameters: List[ConstructorDeclTypeArgument]
    parameters: List[ConstructorDeclArgument]
    return_type: Type


def ConstructorDeclaration_from_cst(cst: Tree) -> ConstructorDeclaration:
    return ConstructorDeclaration(
        name=cst.children[0].value,
        type_parameters=[ConstructorDeclTypeArgument_from_cst(
            typaram) for typaram in cst.children[1].children],
        parameters=[ConstructorDeclArgument_from_cst(
            param) for param in cst.children[2].children],
        return_type=Type_from_cst(cst.children[3]))


@dataclass
class TypeConstructorSignature(Core):
    parameters: List[TyVarKinding]

    def pretty(self, prec=None) -> str:
        return ' '.join([f'({tvk.tyvar} : {tvk.kind.pretty()})' for tvk in self.parameters])


# datatype Data.Pair$Pair (a : Type) (b : Type);;
@dataclass
class DataDeclaration(Declaration):
    name: DeclaredTypeConstructorName
    signature: TypeConstructorSignature
    constructors: List[ConstructorDeclaration]


################  Patterns  ################


@dataclass
class TypeVariablePattern(Core):

    def captured_variables(self) -> List[str]:
        return []


def TypeVariablePattern_from_cst(cst: Tree) -> TypeVariablePattern:
    if cst.data == 'variable_type_pattern':
        return CapturedTypeVariablePattern(var=cst.children[0].value)
    elif cst.data == 'wildcard_type_pattern':
        return WildcardTypeVariablePattern()
    raise

# a
# CapturedTypeVariablePattern("a")


@dataclass
class CapturedTypeVariablePattern(TypeVariablePattern):
    var: str

    def captured_variables(self) -> List[str]:
        return [self.var]

# _
# WildcardTypeVariablePattern()


@dataclass
class WildcardTypeVariablePattern(TypeVariablePattern):

    def captured_variables(self) -> List[str]:
        return []


@dataclass
class Pattern(Core):

    def captured_variables(self) -> List[str]:
        return []


def Pattern_from_cst(cst: Tree) -> Pattern:
    if cst.data == 'variable_pattern':
        return CapturedVariablePattern(var=cst.children[0].value)

    elif cst.data == 'wildcard_pattern':
        return WildcardVariablePattern()

    elif cst.data == 'constructor_pattern':
        return ConstructorPattern(
            constructor=cst.children[0].value,
            type_arguments=[TypeVariablePattern_from_cst(
                pat) for pat in cst.children[1].children],
            arguments=[Pattern_from_cst(pat) for pat in cst.children[2].children])
    raise


# x
# CapturedVariablePattern("x")
@dataclass
class CapturedVariablePattern(Pattern):
    var: str

    def captured_variables(self) -> List[str]:
        return [self.var]

# _
# WildcardVariablePattern()


@dataclass
class WildcardVariablePattern(Pattern):

    def captured_variables(self) -> List[str]:
        return []


# Cons(x; xs)
# ConstructorPattern("Cons", [VariablePattern("x", VariablePattern("xs")])
@dataclass
class ConstructorPattern(Pattern):
    constructor: DeclaredConstructorName
    type_arguments: List[TypeVariablePattern]
    arguments: List[Pattern]

    def captured_variables(self) -> List[str]:
        return [v for pat in self.type_arguments for v in pat.captured_variables()] +\
          [v for pat in self.arguments for v in pat.captured_variables()]


################  Terms  ################

@dataclass
class Term(Core):
    pass


def Term_from_cst(cst: Tree) -> Term:

    if cst.data == 'variable_term':
        return VariableTerm(cst.children[0].value)

    elif cst.data == 'declared_term':
        return DeclaredTermNameTerm(name=DeclaredTermName_from_cst(cst.children[0]))

    elif cst.data == 'type_annotation_term':
        return TypeAnnotationTerm(
            term=Term_from_cst(cst.children[0]),
            type=Type_from_cst(cst.children[1]))

    elif cst.data == 'lambda_term':
        return LambdaTerm(
            body=Scope(
                names=[cst.children[0].value],
                body=Term_from_cst(cst.children[1])))

    elif cst.data == 'application_term':
        return ApplicationTerm(
            function=Term_from_cst(cst.children[0]),
            argument=Term_from_cst(cst.children[1]))

    elif cst.data == 'abstraction_term':
        return AbstractionTerm(
            body=Scope(
                names=[cst.children[0].value],
                body=Term_from_cst(cst.children[1])))

    elif cst.data == 'instantiation_term':
        return InstantiationTerm(
            function=Term_from_cst(cst.children[0]),
            argument=Type_from_cst(cst.children[1]))

    elif cst.data == 'constructor_term':
        return ConstructorTerm(
            constructor=cst.children[0].value,
            type_arguments=[Type_from_cst(ty)
                            for ty in cst.children[1].children],
            arguments=[Term_from_cst(tm) for tm in cst.children[2].children])

    elif cst.data == 'case_term':
        return CaseTerm(
            scrutinees=[Term_from_cst(tm) for tm in cst.children[0].children],
            clauses=[CaseClause_from_cst(cls) for cls in cst.children[1].children])

    elif cst.data == 'paren_term':
        return Term_from_cst(cst.children[0])

    raise


# x
# VariableTerm("x")
@dataclass
class VariableTerm(Term):
    name: str

    def pretty(self, prec=None) -> str:
        return f'{self.name}'

    def rename(self, renaming):
        if renaming == {}:
            return self
        if self.name in renaming:
            return VariableTerm(renaming[self.name])
        else:
            return self


# x : a
# AnnotationTerm(VariableTerm("x"), VariableType("a"))
@dataclass
class TypeAnnotationTerm(Term):
    term: Term
    type: Type


@dataclass
class DeclaredTermNameTerm(Term):
    name: DeclaredTermName


# Pair(x;y)
# ConstructorTerm("Pair", [VariableTerm("x"), VariableTerm("y")])
@dataclass
class ConstructorTerm(Term):
    constructor: DeclaredConstructorName
    type_arguments: List[Type]
    arguments: List[Term]


# \x -> x
# LambdaTerm(Scope(["x"], VariableTerm("x")))
@dataclass
class LambdaTerm(Term):
    body: Scope


# f x
# ApplicationTerm(VariableTerm("f"), VariableTerm("x"))
@dataclass
class ApplicationTerm(Term):
    function: Term
    argument: Term


#\{a} -> x
# AbstractionTerm(Scope(["a"], VariableTerm("x")))
@dataclass
class AbstractionTerm(Term):
    body: Scope


# f {a}
# InstantiationTerm(VariableTerm("f"), VariableType("a"))
@dataclass
class InstantiationTerm(Term):
    function: Term
    argument: Type


@dataclass
class Guard(Core):
    term: Optional[Scope]


def Guard_from_cst(vars: List[str], cst: Tree) -> Guard:
    if len(cst.children) == 0:
        return Guard(term=None)
    else:
        return Guard(term=Scope(
            names=vars,
            body=Term_from_cst(cst.children[0])))


@dataclass
class CaseClause(Core):
    patterns: List[Pattern]
    guard: Guard
    body: Scope


def CaseClause_from_cst(cst: Tree) -> CaseClause:
    pats = [Pattern_from_cst(pat) for pat in cst.children[0].children]
    bound_vars = [v for pat in pats for v in pat.captured_variables()]
    return CaseClause(
        patterns=pats,
        guard=Guard_from_cst(bound_vars, cst.children[1]),
        body=Scope(bound_vars, Term_from_cst(cst.children[2])))


@dataclass
class CaseTerm(Term):
    scrutinees: List[Term]
    clauses: List[CaseClause]


################  Term Declaration  ################


# Data.List$reverse : ... = ...;;
@dataclass
class TermDeclaration(Declaration):
    name: TermNameWithSubnames
    type: Type
    definition: Term


################  Modules  ################


@dataclass
class Module(Core):
    declarations: List[Declaration]


def Module_from_cst(cst: Tree) -> Module:
    return Module(declarations=[Declaration_from_cst(decl) for decl in cst.children])
