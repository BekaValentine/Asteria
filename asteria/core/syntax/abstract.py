# This file defines the core language for Asteria. This is very similar to the Core language but it lacks all of the conveniences and makes all things explicit and uniquely representable.

from dataclasses import dataclass
from lark import Tree, Token
from typing import List, Optional, Tuple, cast

from asteria.utils import *


@dataclass(eq=False)
class Core(Syntax):
    pass

################  Kinds  ################


@dataclass(eq=False)
class Kind(Core):

    def pretty(self, prec=None) -> str:
        return f'(unknown kind: {self})'


def Kind_from_cst(cst: Tree) -> Kind:

    if cst.data == 'type_kind':
        return TypeKind(source=cst)

    elif cst.data == 'function_kind':
        return FunctionKind(
            source=cst,
            argument_kind=Kind_from_cst(cst.children[0]),
            return_kind=Kind_from_cst(cst.children[1]))

    raise

# Type
# TypeKind()


@dataclass(eq=False)
class TypeKind(Kind):

    def pretty(self, prec=None) -> str:
        return 'Type'


# Type -> Type
# FunctionKind(TypeKind(), TypeKind())
@dataclass(eq=False)
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
    return [cast(Token, tok).value for tok in cst.children]


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
        name=cast(Token, cst.children[1]).value)


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
    return DeclaredConstructorName(
        type_name=DeclaredTypeConstructorName_from_cst(cst.children[0]),
        name=cast(Token, cst.children[1]).value)


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
        root_name=cast(Token, cst.children[0]).value,
        sub_names=[cast(Token, name).value for name in cst.children[1].children])


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
@dataclass(eq=False)
class TyVarKinding(Core):
    tyvar: str
    kind: Kind


def TyVarKinding_from_cst(cst: Tree) -> TyVarKinding:
    return TyVarKinding(
        source=cst,
        tyvar=cast(Token, cst.children[0]).value,
        kind=Kind_from_cst(cst.children[1]))


@dataclass(eq=False)
class Type(Core):
    pass


def Type_from_cst(cst: Tree) -> Type:

    if cst.data == 'variable_type':
        return VariableType(
            source=cst,
            name=cast(Token, cst.children[0]).value)

    elif cst.data == 'constructor_type':
        return ConstructorType(
            source=cst,
            name=DeclaredTypeConstructorName_from_cst(cst.children[0]),
            arguments=[Type_from_cst(arg) for arg in cst.children[1].children])

    elif cst.data == 'function_type':
        return FunctionType(
            source=cst,
            argument_type=Type_from_cst(cst.children[0]),
            return_type=Type_from_cst(cst.children[1]))

    elif cst.data == 'forall_type':
        tvk = TyVarKinding_from_cst(cst.children[0])
        return ForallType(
            source=cst,
            tyvar_kind=tvk.kind,
            scope=Scope(
                names=[tvk.tyvar],
                body=Type_from_cst(cst.children[1])))

    elif cst.data == 'lambda_type':
        tvk = TyVarKinding_from_cst(cst.children[0])
        return LambdaType(
            source=cst,
            tyvar_kind=tvk.kind,
            body=Scope(
                names=[tvk.tyvar],
                body=Type_from_cst(cst.children[1])))

    elif cst.data == 'application_type':
        return ApplicationType(
            source=cst,
            function=Type_from_cst(cst.children[0]),
            argument=Type_from_cst(cst.children[1]))

    elif cst.data == 'paren_type':
        return Type_from_cst(cst.children[0])

    raise


# x
# VariableType("")
@dataclass(eq=False)
class VariableType(Type):
    name: str

    def pretty(self, prec=None) -> str:
        return f'{self.name}'

    def rename(self, renaming):
        if renaming == {}:
            return self
        if self.name in renaming:
            return VariableType(
                source=self.source,
                name=renaming[self.name])
        else:
            return self


# Data.Pair$Pair(a;b)
# ConstructorType("Pair", [VariableType("a"), VariableType("b")])
@dataclass(eq=False)
class ConstructorType(Type):
    name: DeclaredTypeConstructorName
    arguments: List[Type]

    def pretty(self, prec=None) -> str:
        return self.name.pretty() + '(' + ','.join([t.pretty() for t in self.arguments]) + ')'


# a -> b
# FunctionType(VariableType("a"), VariableType("b"))
@dataclass(eq=False)
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
@dataclass(eq=False)
class ForallType(Type):
    tyvar_kind: Kind
    scope: Scope[Type]

    def pretty(self, prec=None) -> str:
        p = f'forall ({self.scope.names[0]} : {self.tyvar_kind.pretty()}). {self.scope.body.pretty()}'
        if prec == 'argument':
            return f'({p})'
        else:
            return p


# \(a : Type) -> a
# LambdaType(TypeKind(), Scope(["a"], VariableType("a")))
@dataclass(eq=False)
class LambdaType(Type):
    tyvar_kind: Kind
    body: Scope[Type]

    def pretty(self, prec=None) -> str:
        p = f'\({self.body.names[0]} : {self.tyvar_kind.pretty()}) -> {self.body.body.pretty()}'
        if prec in ['argument', 'function', 'argument']:
            return f'({p})'
        else:
            return p


# f $ a
# ApplicationType(VariableType("f"), VariableType("a"))
@dataclass(eq=False)
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

@dataclass(eq=False)
class Declaration(Core):
    pass


def Declaration_from_cst(cst: Tree) -> Declaration:
    if cst.data == 'data_declaration':
        params = [TyVarKinding_from_cst(tvk)
                  for tvk in cst.children[1].children]
        return DataDeclaration(
            source=cst,
            name=cast(Token, cst.children[0]).value,
            signature=TypeConstructorSignature(
                source=None,
                parameters=params),
            constructors=[ConstructorDeclaration_from_cst([tvk.tyvar for tvk in params], condecl) for condecl in cst.children[2].children])
    elif cst.data == 'term_declaration':
        return TermDeclaration(
            source=cst,
            name=TermNameWithSubnames_from_cst(cst.children[0]),
            type=Type_from_cst(cst.children[1]),
            definition=Term_from_cst(cst.children[2]))

    raise


################  Data Declarations  ################


@dataclass(eq=False)
class ConstructorDeclTypeArgument(Core):
    tyvar: str
    kind: Kind


def ConstructorDeclTypeArgument_from_cst(cst: Tree) -> ConstructorDeclTypeArgument:
    return ConstructorDeclTypeArgument(
        source=cst,
        tyvar=cast(Token, cst.children[0]).value,
        kind=Kind_from_cst(cst.children[1]))


@dataclass(eq=False)
class ConstructorDeclArgument(Core):
    var: str
    type: Type


def ConstructorDeclArgument_from_cst(cst: Tree) -> ConstructorDeclArgument:
    return ConstructorDeclArgument(
        source=cst,
        var=cast(Token, cst.children[0]).value,
        type=Type_from_cst(cst.children[1]))


@dataclass(eq=False)
class ConstructorTermSignature(Core):
    parameters: List[Tuple[str, Type]]
    return_type: Type


@dataclass(eq=False)
class ConstructorSignature(Core):
    type_parameters_kinds: List[Kind]
    term_signature: Scope[ConstructorTermSignature]

    def pretty(self, prec=None) -> str:
        ts = self.term_signature.body
        return ' '.join([f'{{{v} : {k.pretty()}}}' for v, k in zip(self.term_signature.names, self.type_parameters_kinds)]) +\
               ' ' +\
               ' '.join([f'({v} : {k.pretty()})' for v, k in ts.parameters]) +\
               ' ~> ' +\
               ts.return_type.pretty()


# constructor Data.Pair$Pair$MkPair {a : Type} {b : Type} (x : a) (y : b) : Data.Pair$Pair(a;b);;
@dataclass(eq=False)
class ConstructorDeclaration(Core):
    name: str
    signature: Scope[ConstructorSignature]


def ConstructorDeclaration_from_cst(tycon_params: List[str], cst: Tree) -> ConstructorDeclaration:
    type_params = [ConstructorDeclTypeArgument_from_cst(
        typaram) for typaram in cst.children[1].children]
    params = [ConstructorDeclArgument_from_cst(
        param) for param in cst.children[2].children]

    return ConstructorDeclaration(
            source=cst,
            name=cast(Token, cst.children[0]).value,
            signature=Scope(
                names=tycon_params,
                body=ConstructorSignature(
                    source=None,
                    type_parameters_kinds=[tp.kind for tp in type_params],
                    term_signature=Scope(
                        names=[tp.tyvar for tp in type_params],
                        body=ConstructorTermSignature(
                            source=None,
                            parameters=[(p.var, p.type) for p in params],
                            return_type=Type_from_cst(cst.children[3]))))))


@dataclass(eq=False)
class TypeConstructorSignature(Core):
    parameters: List[TyVarKinding]

    def pretty(self, prec=None) -> str:
        return ' '.join([f'({tvk.tyvar} : {tvk.kind.pretty()})' for tvk in self.parameters])


# datatype Data.Pair$Pair (a : Type) (b : Type);;
@dataclass(eq=False)
class DataDeclaration(Declaration):
    name: str
    signature: TypeConstructorSignature
    constructors: List[ConstructorDeclaration]


################  Patterns  ################


@dataclass(eq=False)
class TypeVariablePattern(Core):

    def captured_variables(self) -> List[str]:
        return []


def TypeVariablePattern_from_cst(cst: Tree) -> TypeVariablePattern:
    if cst.data == 'variable_type_pattern':
        return CapturedTypeVariablePattern(
            source=cst,
            tyvar=cast(Token, cst.children[0]).value)
    elif cst.data == 'wildcard_type_pattern':
        return WildcardTypeVariablePattern(source=cst)
    raise

# a
# CapturedTypeVariablePattern("a")


@dataclass(eq=False)
class CapturedTypeVariablePattern(TypeVariablePattern):
    tyvar: str

    def captured_variables(self) -> List[str]:
        return [self.tyvar]

# _
# WildcardTypeVariablePattern()


@dataclass(eq=False)
class WildcardTypeVariablePattern(TypeVariablePattern):

    def captured_variables(self) -> List[str]:
        return []


@dataclass(eq=False)
class Pattern(Core):

    def captured_variables(self) -> List[str]:
        return []


def Pattern_from_cst(cst: Tree) -> Pattern:
    if cst.data == 'variable_pattern':
        return CapturedVariablePattern(
            source=cst,
            var=cast(Token, cst.children[0]).value)

    elif cst.data == 'wildcard_pattern':
        return WildcardVariablePattern(source=cst)

    elif cst.data == 'constructor_pattern':
        return ConstructorPattern(
            source=cst,
            constructor=cast(Token, cst.children[0]).value,
            type_arguments=[TypeVariablePattern_from_cst(
                pat) for pat in cst.children[1].children],
            arguments=[Pattern_from_cst(pat) for pat in cst.children[2].children])
    raise


# x
# CapturedVariablePattern("x")
@dataclass(eq=False)
class CapturedVariablePattern(Pattern):
    var: str

    def captured_variables(self) -> List[str]:
        return [self.var]

# _
# WildcardVariablePattern()


@dataclass(eq=False)
class WildcardVariablePattern(Pattern):

    def captured_variables(self) -> List[str]:
        return []


# Cons(x; xs)
# ConstructorPattern("Cons", [VariablePattern("x", VariablePattern("xs")])
@dataclass(eq=False)
class ConstructorPattern(Pattern):
    constructor: str
    type_arguments: List[TypeVariablePattern]
    arguments: List[Pattern]

    def captured_variables(self) -> List[str]:
        return [v for pat in self.type_arguments for v in pat.captured_variables()] +\
          [v for pat in self.arguments for v in pat.captured_variables()]


################  Terms  ################

@dataclass(eq=False)
class Term(Core):
    pass


def Term_from_cst(cst: Tree) -> Term:

    if cst.data == 'variable_term':
        return VariableTerm(
            source=cst,
            name=cast(Token, cst.children[0]).value)

    elif cst.data == 'declared_term':
        return DeclaredTermNameTerm(
            source=cst,
            name=DeclaredTermName_from_cst(cst.children[0]))

    elif cst.data == 'type_annotation_term':
        return TypeAnnotationTerm(
            source=cst,
            term=Term_from_cst(cst.children[0]),
            type=Type_from_cst(cst.children[1]))

    elif cst.data == 'lambda_term':
        return LambdaTerm(
            source=cst,
            body=Scope(
                names=[cast(Token, cst.children[0]).value],
                body=Term_from_cst(cst.children[1])))

    elif cst.data == 'application_term':
        return ApplicationTerm(
            source=cst,
            function=Term_from_cst(cst.children[0]),
            argument=Term_from_cst(cst.children[1]))

    elif cst.data == 'abstraction_term':
        return AbstractionTerm(
            source=cst,
            body=Scope(
                names=[cast(Token, cst.children[0]).value],
                body=Term_from_cst(cst.children[1])))

    elif cst.data == 'instantiation_term':
        return InstantiationTerm(
            source=cst,
            function=Term_from_cst(cst.children[0]),
            argument=Type_from_cst(cst.children[1]))

    elif cst.data == 'constructor_term':
        return ConstructorTerm(
            source=cst,
            constructor=cast(Token, cst.children[0]).value,
            type_arguments=[Type_from_cst(ty)
                            for ty in cst.children[1].children],
            arguments=[Term_from_cst(tm) for tm in cst.children[2].children])

    elif cst.data == 'case_term':
        return CaseTerm(
            source=cst,
            scrutinees=[Term_from_cst(tm) for tm in cst.children[0].children],
            clauses=[CaseClause_from_cst(cls) for cls in cst.children[1].children])

    elif cst.data == 'paren_term':
        return Term_from_cst(cst.children[0])

    raise


# x
# VariableTerm("x")
@dataclass(eq=False)
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
@dataclass(eq=False)
class TypeAnnotationTerm(Term):
    term: Term
    type: Type


@dataclass(eq=False)
class DeclaredTermNameTerm(Term):
    name: DeclaredTermName


# Pair(x;y)
# ConstructorTerm("Pair", [VariableTerm("x"), VariableTerm("y")])
@dataclass(eq=False)
class ConstructorTerm(Term):
    constructor: str
    type_arguments: List[Type]
    arguments: List[Term]


# \x -> x
# LambdaTerm(Scope(["x"], VariableTerm("x")))
@dataclass(eq=False)
class LambdaTerm(Term):
    body: Scope[Term]


# f x
# ApplicationTerm(VariableTerm("f"), VariableTerm("x"))
@dataclass(eq=False)
class ApplicationTerm(Term):
    function: Term
    argument: Term


#\{a} -> x
# AbstractionTerm(Scope(["a"], VariableTerm("x")))
@dataclass(eq=False)
class AbstractionTerm(Term):
    body: Scope[Term]


# f {a}
# InstantiationTerm(VariableTerm("f"), VariableType("a"))
@dataclass(eq=False)
class InstantiationTerm(Term):
    function: Term
    argument: Type


@dataclass(eq=False)
class CaseClause(Core):
    patterns: List[Pattern]
    body: Scope[Term]


def CaseClause_from_cst(cst: Tree) -> CaseClause:
    pats = [Pattern_from_cst(pat) for pat in cst.children[0].children]
    bound_vars = [v for pat in pats for v in pat.captured_variables()]
    return CaseClause(
        source=cst,
        patterns=pats,
        body=Scope(bound_vars, Term_from_cst(cst.children[1])))


@dataclass(eq=False)
class CaseTerm(Term):
    scrutinees: List[Term]
    clauses: List[CaseClause]


################  Term Declaration  ################


# Data.List$reverse : ... = ...;;
@dataclass(eq=False)
class TermDeclaration(Declaration):
    name: TermNameWithSubnames
    type: Type
    definition: Term


################  Modules  ################


@dataclass(eq=False)
class Module(Core):
    declarations: List[Declaration]


def Module_from_cst(cst: Tree) -> Module:
    return Module(
        source=cst,
        declarations=[Declaration_from_cst(decl) for decl in cst.children])
