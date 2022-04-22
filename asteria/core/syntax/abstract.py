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
    pass


def Kind_from_cst(cst: Tree) -> Kind:

    if cst.data == 'type_kind':
        return TypeKind(source=cst)

    elif cst.data == 'function_kind':
        return FunctionKind(
            source=cst,
            argument_kind=Kind_from_cst(cst.children[0]),
            return_kind=Kind_from_cst(cst.children[1]))

    raise


# x
# MetaSyntacticVariableKind("x")
@dataclass(eq=False)
class MetaSyntacticVariableKind(Kind):
    name: str

    def concrete(self, name):
        return name

# Type
# TypeKind()


@dataclass(eq=False)
class TypeKind(Kind):
    def concrete(self):
        return 'Type'


# k0 -> k1
# FunctionKind(k0,k1)
@dataclass(eq=False)
class FunctionKind(Kind):
    parens = ['function_kind.argument_kind']
    argument_kind: Kind
    return_kind: Kind

    def concrete(self, arg, ret):
        return f'{arg} -> {ret}'


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


# a : k
# TyVarkinding("a", k)
@dataclass(eq=False)
class TyVarKinding(Core):
    tyvar: str
    kind: Kind

    def concrete(self, v, k):
        return f'({v} : {k})'


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
# MetaSyntacticVariableType("x")
@dataclass(eq=False)
class MetaSyntacticVariableType(Type):
    name: str

    def rename(self, renaming):
        if renaming == {}:
            return self
        if self.name in renaming:
            return VariableType(
                source=self.source,
                name=renaming[self.name])
        else:
            return self

    def concrete(self, name):
        return name


# x
# VariableType("x")
@dataclass(eq=False)
class VariableType(Type):
    name: str

    def rename(self, renaming):
        if renaming == {}:
            return self
        if self.name in renaming:
            return VariableType(
                source=self.source,
                name=renaming[self.name])
        else:
            return self

    def concrete(self, name):
        return name


# c(a0,...,an)
# ConstructorType(c, [a0, ..., an])
@dataclass(eq=False)
class ConstructorType(Type):
    name: DeclaredTypeConstructorName
    arguments: List[Type]

    def concrete(self, name, args):
        return f'{name}({",".join(args)})'


# a -> b
# FunctionType(a, b)
@dataclass(eq=False)
class FunctionType(Type):
    parens = [
        'function_type.argument_type',
        'application_type.function',
        'application_type.argument',
    ]
    argument_type: Type
    return_type: Type

    def concrete(self, arg, ret):
        return f'{arg} -> {ret}'


# forall (a : k). body
# ForallType(k, Scope(["a"], body))
@dataclass(eq=False)
class ForallType(Type):
    parens = [
        'function_type.argument_type',
        'application_type.function',
        'application_type.argument',
    ]
    tyvar_kind: Kind
    scope: Scope[Type]

    def concrete(self, k, sc):
        [name], body = sc
        return f'forall ({name} : {k}). {body}'


# \(a : k) -> body
# LambdaType(k, Scope(["a"], body))
@dataclass(eq=False)
class LambdaType(Type):
    parens = [
        'function_type.argument_type',
        'application_type.function',
        'application_type.argument',
    ]
    tyvar_kind: Kind
    body: Scope[Type]

    def concrete(self, k, sc):
        [name], body = sc
        return f'\({name} : {k}) -> {body}'


# f a
# ApplicationType(f, a)
@dataclass(eq=False)
class ApplicationType(Type):
    parens = [
        'application_type.argument',
    ]
    function: Type
    argument: Type

    def concrete(self, f, a):
        return f'{f} {a}'


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

    def concrete(self, ps, ret):
        if len(ps) == 0:
            return f': {ret}'
        else:
            psp = ' '.join([
                f'({n} : {t})'
                for n, t in ps
            ])
            return f'{psp} : {ret}'


@dataclass(eq=False)
class ConstructorSignature(Core):
    type_parameters_kinds: List[Kind]
    term_signature: Scope[ConstructorTermSignature]

    def concrete(self, ks, tsigsc):
        ns, tsig = tsigsc

        if len(ks) == 0:
            return tsig
        else:
            typarams = ' '.join([
                f'{{{n} : {k}}}'
                for n, k in zip(ns, ks)
            ])
            return f'{typarams} {tsig}'


# | MkPair {a : Type} {b : Type} (x : a) (y : b) : Data.Pair$Pair(a;b);;
@dataclass(eq=False)
class ConstructorDeclaration(Core):
    name: str
    signature: Scope[ConstructorSignature]

    def concrete(self, name, sigsc):
        _, sig = sigsc
        return f'{name} {sig}'


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

    def concrete(self, ps):
        return ' '.join(ps)


# data Pair (a : Type) (b : Type) where ...;;
@dataclass(eq=False)
class DataDeclaration(Declaration):
    name: str
    signature: TypeConstructorSignature
    constructors: List[ConstructorDeclaration]

    def concrete(self, name, sig, cons):
        consp = ''.join([
            f'\n   | {c}'
            for c in cons
        ])

        if sig == '':
            sigp = ''
        else:
            sigp = f' {sig}'
        return f'data {name}{sigp} where{consp}\n   ;;'


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

    def concrete(self, n):
        return n

# _
# WildcardVariablePattern()


@dataclass(eq=False)
class WildcardVariablePattern(Pattern):

    def captured_variables(self) -> List[str]:
        return []

    def concrete(self):
        return '_'


# c(a0, ..., am; x0, ... an)
# ConstructorPattern(c, [a0, ..., am], [x0, ..., xn])
@dataclass(eq=False)
class ConstructorPattern(Pattern):
    constructor: str
    type_arguments: List[TypeVariablePattern]
    arguments: List[Pattern]

    def captured_variables(self) -> List[str]:
        return [v for pat in self.type_arguments for v in pat.captured_variables()] +\
          [v for pat in self.arguments for v in pat.captured_variables()]

    def concrete(self, c, targs, args):
        targsp = ','.join(targs)
        argsp = ','.join(args)
        return f'{c}({targsp};{argsp})'


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

    def rename(self, renaming):
        if renaming == {}:
            return self
        if self.name in renaming:
            return VariableTerm(
                source=self.source,
                name=renaming[self.name])
        else:
            return self

    def concrete(self, n):
        return n


# x : a
# AnnotationTerm(VariableTerm("x"), VariableType("a"))
@dataclass(eq=False)
class TypeAnnotationTerm(Term):
    parens = [
        'type_annotation_term.term',
        'application_term.function',
        'application_term.argument',
        'instantiation_term.function',
    ]
    term: Term
    type: Type

    def concrete(self, m, t):
        return f'{m} : {t}'


@dataclass(eq=False)
class DeclaredTermNameTerm(Term):
    name: DeclaredTermName

    def concrete(self, n):
        return n


# c(a0, ..., am; x0, ..., xn)
# ConstructorTerm(c, [a0, ..., am], [x0, ..., xn])
@dataclass(eq=False)
class ConstructorTerm(Term):
    constructor: str
    type_arguments: List[Type]
    arguments: List[Term]

    def concrete(self, c, tyargs, args):
        return f'{c}({",".join(tyargs)};{",".join(args)})'


# \x -> body
# LambdaTerm(Scope(["x"], body))
@dataclass(eq=False)
class LambdaTerm(Term):
    parens = [
        'type_annotation_term.term',
        'application_term.function',
        'application_term.argument',
        'instantiation_term.function',
    ]
    body: Scope[Term]

    def concrete(self, sc):
        [n], b = sc
        return f'\\{n} -> {b}'


# f x
# ApplicationTerm(f, x)
@dataclass(eq=False)
class ApplicationTerm(Term):
    parens = [
        'application_term.argument',
    ]
    function: Term
    argument: Term

    def concrete(self, fun, arg):
        return f'{fun} {arg}'


#\{a} -> body
# AbstractionTerm(Scope(["a"], body))
@dataclass(eq=False)
class AbstractionTerm(Term):
    parens = [
        'type_annotation_term.term',
        'application_term.function',
        'application_term.argument',
        'instantiation_term.function',
    ]
    body: Scope[Term]

    def concrete(self, sc):
        [n], b = sc
        return f'\\{{{n}}} -> {b}'


# f {a}
# InstantiationTerm(f, a)
@dataclass(eq=False)
class InstantiationTerm(Term):
    parens = [
        'application_term.argument'
    ]
    function: Term
    argument: Type

    def concrete(self, fun, arg):
        return f'{fun} {{{arg}}}'


# | p0 | ... | pn -> body
# CaseClause([p0, ..., pn], body)
@dataclass(eq=False)
class CaseClause(Core):
    patterns: List[Pattern]
    body: Scope[Term]

    def concrete(self, ps, bsc):
        psp = ' | '.join(ps)
        _, b = bsc
        return f'| {psp} -> {b}'


def CaseClause_from_cst(cst: Tree) -> CaseClause:
    pats = [Pattern_from_cst(pat) for pat in cst.children[0].children]
    bound_vars = [v for pat in pats for v in pat.captured_variables()]
    return CaseClause(
        source=cst,
        patterns=pats,
        body=Scope(bound_vars, Term_from_cst(cst.children[1])))


# case s0 | ... | sn where clauses;;
# CaseTerm([s0, ..., sn], clauses)
@dataclass(eq=False)
class CaseTerm(Term):
    scrutinees: List[Term]
    clauses: List[CaseClause]

    def concrete(self, scruts, cls):
        scrutsp = ' | '.join(scruts)
        clsp = '\n'.join(cls)
        return f'case {scrutsp} where\n{clsp}\n;;'


################  Term Declaration  ################


# Data.List$reverse : ... = ...;;
@dataclass(eq=False)
class TermDeclaration(Declaration):
    name: TermNameWithSubnames
    type: Type
    definition: Term

    def concrete(self, name, type, definition):
        indent = len(name)*' '
        return f'{name} : {type}\n{indent} = {definition}\n{indent} ;;'


################  Modules  ################


@dataclass(eq=False)
class Module(Core):
    declarations: List[Declaration]

    def concrete(self, declarations):
        return '\n\n'.join(declarations)


def Module_from_cst(cst: Tree) -> Module:
    return Module(
        source=cst,
        declarations=[Declaration_from_cst(decl) for decl in cst.children])
