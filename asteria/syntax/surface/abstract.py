# This module defines the abstract syntax for Asteria's surface language. Very little of interest is happening in this file, except the definition of some types, plus the conversions from concrete syntax trees produced by Lark.

# The surface language has a number of user-friendly conveniences that make it nicer to program in Asteria. Among them are implicit instantiation of forall types, as well as implicit instance arguments. Beyond that, there are conveniences such as constructs as functions and multiple variables for any given binder.

from dataclasses import dataclass
from lark import Tree
from typing import List, Optional, TypeVar


@dataclass
class Surface(object):
    pass

################  Kinds  ################


@dataclass
class Kind(Surface):
    pass


def Kind_from_cst(cst) -> Kind:
    if cst.data == 'type_kind':
        return TypeKind()
    elif cst.data == 'arrow_kind':
        return ArrowKind(Kind_from_cst(cst.children[0]), Kind_from_cst(cst.children[1]))
    elif cst.data == 'paren_kind':
        return Kind_from_cst(cst.children[0])
    raise


# Type
# TypeKind()
@dataclass
class TypeKind(Kind):
    pass


# k1 -> k2
# ArrowKind(k1, k2)
@dataclass
class ArrowKind(Kind):
    argument_kind: Kind
    return_kind: Kind


################  Types  ################

@dataclass
class TyVarKinding(Surface):
    tyvar: List[str]
    kind: Kind


def TyVarKinding_from_cst(cst) -> TyVarKinding:
    return TyVarKinding([tok.value for tok in cst.children[0].children], Kind_from_cst(cst.children[1]))


@dataclass
class Type(Surface):
    pass


def Type_from_cst(cst) -> Type:
    if cst.data == 'type_variable':
        return VariableType(cst.children[0].value)
    elif cst.data == 'type_name':
        return TypeConstructorType(cst.children[0].value)
    elif cst.data == 'function_type':
        return FunctionType(Type_from_cst(cst.children[0]), Type_from_cst(cst.children[1]))
    elif cst.data == 'constrained_type':
        return ConstrainedType([ClassConstraint_from_cst(con) for con in cst.children[0].children],
                               Type_from_cst(cst.children[1]))
    elif cst.data == 'forall_type':
        return ForallType([TyVarKinding_from_cst(tvk) for tvk in cst.children[0].children],
                          Type_from_cst(cst.children[1]))
    elif cst.data == 'type_application':
        return ApplicationType(Type_from_cst(cst.children[0]), Type_from_cst(cst.children[1]))
    elif cst.data == 'paren_type':
        return Type_from_cst(cst.children[0])

    raise


# a
# VariableType('a')
@dataclass
class VariableType(Type):
    name: str


# List
# TypeConstructorType('List')
@dataclass
class TypeConstructorType(Type):
    name: str


# t1 -> t2
# FunctionType(t1, t2)
@dataclass
class FunctionType(Type):
    argument_type: Type
    return_type: Type


@dataclass
class ClassConstraint(Surface):
    name: str
    arguments: List[Type]


def ClassConstraint_from_cst(cst) -> ClassConstraint:
    return ClassConstraint(cst.children[0].value,
                           [Type_from_cst(ty) for ty in cst.children[1].children])


# (Functor f) => t
# ConstrainedType([ClassConstraint("Functor", f)], t)
@dataclass
class ConstrainedType(Type):
    class_constraints: List[ClassConstraint]
    return_type: Type


# forall (f : k). t
# ForallType([TyVarKinding(["f"], k)], t)
@dataclass
class ForallType(Type):
    tyvar_kindings: List[TyVarKinding]
    scope: Type


# f a
# ApplicationType(f, a)
@dataclass
class ApplicationType(Type):
    function: Type
    argument: Type


################  Declarations  ################

@dataclass
class Declaration(Surface):
    pass


def Declaration_from_cst(cst) -> Declaration:

    if cst.data == 'fixity_declaration':
        return FixityDeclaration(fixity=cst.children[0].value,
                                 fixity_level=cst.children[1].value,
                                 operators=[op.value for op in cst.children[2].children])

    elif cst.data == 'data_declaration':
        return DataDeclaration(name=cst.children[0].value,
                               tyvar_kindings=[TyVarKinding_from_cst(
                                   tvk) for tvk in cst.children[1].children],
                               constructor_declarations=[ConstructorDeclaration_from_cst(cdecl) for cdecl in cst.children[2].children])

    elif cst.data == 'type_synonym_declaration':
        return TypeSynonymDeclaration(name=cst.children[0].value,
                                      tyvar_kindings=[TyVarKinding_from_cst(
                                          tvk) for tvk in cst.children[1].children],
                                      definition=Type_from_cst(cst.children[2]))
    elif cst.data == 'type_class_declaration':
        return TypeClassDeclaration(class_quantification=[TyVarKinding_from_cst(tvk) for tvk in cst.children[0].children[0].children],
                                    class_precondition=[ClassConstraint_from_cst(
                                        con) for con in cst.children[1].children],
                                    name=cst.children[2].value,
                                    class_params=[
                                        tok.value for tok in cst.children[3].children],
                                    method_declarations=[MethodDeclaration_from_cst(mdecl) for mdecl in cst.children[4].children])
    elif cst.data == 'type_signature':
        return TypeSignature(name=cst.children[0].value,
                             type=Type_from_cst(cst.children[1]))
    elif cst.data == 'term_equation':
        return TermEquation(name=cst.children[0].value,
                            patterns=[Pattern_from_cst(pat)
                                      for pat in cst.children[1].children],
                            guard=Guard_from_cst(cst.children[2]),
                            definition=Term_from_cst(cst.children[3]))
    elif cst.data == 'instance_declaration':
        if len(cst.children[0].children) > 0:
            class_quantification_tvks = cst.children[0].children[0].children[0].children
        else:
            class_quantification_tvks = []

        return InstanceDeclaration(instance_quantification=[TyVarKinding_from_cst(tvk) for tvk in class_quantification_tvks],
                                   instance_precondition=[ClassConstraint_from_cst(
                                       con) for con in cst.children[1].children],
                                   name=cst.children[2].value,
                                   instance_parameters=[Type_from_cst(ty)
                                                        for ty in cst.children[3].children],
                                   method_equations=[MethodEquation_from_cst(meq) for meq in cst.children[4].children])

    raise


################  Fixity Declarations  ################

@dataclass
class Fixity(Surface):
    pass


def Fixity_from_cst(cst) -> Fixity:
    if cst.data == 'infix':
        return Infix()
    elif cst.data == 'infixl':
        return InfixL()
    elif cst.data == 'infixr':
        return InfixR()
    raise


@dataclass
class Infix(Fixity):
    pass


@dataclass
class InfixL(Fixity):
    pass


@dataclass
class InfixR(Fixity):
    pass


# infix 5 *
# FixityDeclaration(Infix(), 5, ["*"])
@dataclass
class FixityDeclaration(Declaration):
    fixity: Fixity
    fixity_level: int
    operators: List[str]


################  Data Declarations  ################


@dataclass
class ConstructorTypeParameter(Surface):
    tyvars: List[str]
    kind: Kind


def ConstructorTypeParameter_from_cst(cst) -> ConstructorTypeParameter:
    return ConstructorTypeParameter([tok.value for tok in cst.children[0].children], Kind_from_cst(cst.children[1]))


@dataclass
class ConstructorParameter(Surface):
    vars: List[str]
    type: Type


def ConstructorParameter_from_cst(cst) -> ConstructorParameter:
    return ConstructorParameter([tok.value for tok in cst.children[0].children], Type_from_cst(cst.children[1]))


@dataclass
class ConstructorDeclaration(Surface):
    name: str
    type_parameters: List[ConstructorTypeParameter]
    parameters: List[ConstructorParameter]
    return_type: Type


def ConstructorDeclaration_from_cst(cst) -> ConstructorDeclaration:
    return ConstructorDeclaration(cst.children[0].value,
                                  [ConstructorTypeParameter_from_cst(
                                      tvk) for tvk in cst.children[1].children],
                                  [ConstructorParameter_from_cst(
                                      vt) for vt in cst.children[2].children],
                                  Type_from_cst(cst.children[3]))


# data Maybe (a : Type) where
#   | Nothing : Maybe a
#   | Just (x : a) : Maybe a
#   ;;
# DataDeclaration(
#   "Maybe",
#   TyVarKinding(["a"], TypeKind()),
#   [
#     ConstructorDeclaration(
#       "Nothing",
#       [],
#       [],
#       ApplicationType(TypeConstructorType("Maybe"), VariableType("a"))
#     ),
#     ConstructorDeclaration(
#       "Just",
#       [],
#       [ConstructorParameter(["x"], TypeVariable("a"))],
#       ApplicationType(TypeConstructorType("Maybe"), VariableType("a"))
#     )
#   ])
@dataclass
class DataDeclaration(Declaration):
    name: str
    tyvar_kindings: List[TyVarKinding]
    constructor_declarations: List[ConstructorDeclaration]


################  Type Synonym Declarations  ################

# type t1 = t2;;
# TypeSynonymDeclaration(t1,t2)
@dataclass
class TypeSynonymDeclaration(Declaration):
    name: str
    tyvar_kindings: List[TyVarKinding]
    definition: Type


################  Type Class Declarations  ################

@dataclass
class MethodDeclaration(Surface):
    name: str
    type: Type


def MethodDeclaration_from_cst(cst) -> MethodDeclaration:
    return MethodDeclaration(cst.children[0].value,
                             Type_from_cst(cst.children[1]))


# class forall (a : k). C a where
#   | m : t
#   ;;
# TypeClassDeclaration(
#   [TyVarKinding(["a"], k)],
#   [],
#   "C",
#   [VariableType("a")],
#   [
#     MethodDeclaration("m", t)
#   ]
# )
@dataclass
class TypeClassDeclaration(Declaration):
    class_quantification: List[TyVarKinding]
    class_precondition: List[ClassConstraint]
    name: str
    class_params: List[str]
    method_declarations: List[MethodDeclaration]


################  Type Signature  ################


# x : t;;
# TypeSignature("x", t)
@dataclass
class TypeSignature(Declaration):
    name: str
    type: Type


################  Patterns  ################

@dataclass
class Pattern(Surface):
    pass


def Pattern_from_cst(cst) -> Pattern:
    if cst.data == 'variable_pattern':
        return VariablePattern(cst.children[0].value)
    elif cst.data == 'wildcard_pattern':
        return WildcardPattern()
    elif cst.data == 'constructor_pattern':
        return ConstructorPattern(cst.children[0].value,
                                  [Pattern_from_cst(pat) for pat in cst.children[1].children])
    elif cst.data == 'paren_pattern':
        return Pattern_from_cst(cst.children[0])
    raise


# x
# VariablePattern("x")
@dataclass
class VariablePattern(Pattern):
    name: str


# _
# WildcardPattern()
@dataclass
class WildcardPattern(Pattern):
    pass


#  Cons p1 p2
# ConstructorPattern("Cons", [p1,p2])
@dataclass
class ConstructorPattern(Pattern):
    constructor: str
    arguments: List[Pattern]


################  Terms  ################

@dataclass
class Term(Surface):
    pass


def Term_from_cst(cst) -> Term:
    if cst.data == 'type_annotation':
        return TypeAnnotationTerm(Term_from_cst(cst.children[0]),
                                  Type_from_cst(cst.children[1]))
    elif cst.data == 'variable':
        return VariableTerm(cst.children[0].value)
    elif cst.data == 'constructor':
        return ConstructorTerm(cst.children[0].value)
    elif cst.data == 'lambda':
        return LambdaTerm([FormalParameter_from_cst(par) for par in cst.children[0].children],
                          Term_from_cst(cst.children[1]))
    elif cst.data == 'application':
        return ApplicationTerm(Term_from_cst(cst.children[0]),
                               ApplicationArgument_from_cst(cst.children[1]))
    elif cst.data == 'case':
        return CaseTerm([Term_from_cst(c) for c in cst.children[0].children],
                        [CaseClause_from_cst(cls) for cls in cst.children[1].children])
    elif cst.data == 'let':
        return LetTerm([LetDeclaration_from_cst(decl) for decl in cst.children[0].children],
                       Term_from_cst(cst.children[1]))
    elif cst.data == 'infix':
        return InfixTerm(cst.children[1].value,
                         Term_from_cst(cst.children[0]),
                         Term_from_cst(cst.children[2]))
    elif cst.data == 'infix_operator':
        return InfixOperatorTerm(cst.children[0].value)
    elif cst.data == 'left_section':
        return LeftSectionTerm(cst.children[1].value,
                               Term_from_cst(cst.children[0]))
    elif cst.data == 'right_section':
        return RightSectionTerm(cst.children[0].value,
                                Term_from_cst(cst.children[1]))
    elif cst.data == 'paren_term':
        return Term_from_cst(cst.children[0])
    raise


@dataclass
class Guard(Surface):
    term: Optional[Term]


def Guard_from_cst(cst) -> Guard:
    if len(cst.children) == 0:
        return Guard(None)
    else:
        return Guard(Term_from_cst(cst.children[0]))


# m : t
# TypeAnnotationTerm(m, t)
@dataclass
class TypeAnnotationTerm(Term):
    term: Term
    type: Type


# x
# VariableTerm("x")
@dataclass
class VariableTerm(Term):
    name: str


# Just
# ConstructorTerm("Just")
@dataclass
class ConstructorTerm(Term):
    constructor: str


@dataclass
class VarParameter(Surface):
    pass


def VarParameter_from_cst(cst):
    if cst.data == 'named_var_parameter':
        return NamedVarParameter(cst.children[0].value)
    elif cst.data == 'wildcard_var_parameter':
        return WildcardVarParameter()


@dataclass
class NamedVarParameter(VarParameter):
    name: str


@dataclass
class WildcardVarParameter(VarParameter):
    pass


@dataclass
class TyVarParameter(Surface):
    pass


def TyVarParameter_from_cst(cst):
    if cst.data == 'named_tyvar_parameter':
        return NamedTyVarParameter(cst.children[0].value)
    elif cst.data == 'wildcard_tyvar_parameter':
        return WildcardTyVarParameter()


@dataclass
class NamedTyVarParameter(TyVarParameter):
    name: str


@dataclass
class WildcardTyVarParameter(TyVarParameter):
    pass


@dataclass
class FormalParameter(Surface):
    pass


def FormalParameter_from_cst(cst) -> FormalParameter:
    if cst.data == 'untyped_variable_parameter':
        return UntypedVariableParameter(VarParameter_from_cst(cst.children[0]))
    elif cst.data == 'typed_variable_parameter':
        return TypedVariableParameter([VarParameter_from_cst(c) for c in cst.children[0].children],
                                      Type_from_cst(cst.children[1]))
    elif cst.data == 'unkinded_type_variable_parameter':
        return UnkindedTyVarParameter(TyVarParameter_from_cst(cst.children[0]))
    elif cst.data == 'kinded_type_variable_parameter':
        return KindedTyVarParameter([TyVarParameter_from_cst(c) for c in cst.children[0].children],
                                    Kind_from_cst(cst.children[1]))
    raise


@dataclass
class UntypedVariableParameter(FormalParameter):
    name: str


@dataclass
class TypedVariableParameter(FormalParameter):
    name: List[str]
    type: Type


@dataclass
class UnkindedTyVarParameter(FormalParameter):
    name: str


@dataclass
class KindedTyVarParameter(FormalParameter):
    name: List[str]
    kind: Kind


# \{a} (x : t) -> m
# LambdaTerm(
#   [
#     UnkindedTyVarParameter("a"),
#     TypedVariableParameter(["x"], t)
#   ],
#   m
# )
@dataclass
class LambdaTerm(Term):
    formal_parameters: List[FormalParameter]
    body: Term


@dataclass
class ApplicationArgument(Surface):
    pass


def ApplicationArgument_from_cst(cst) -> ApplicationArgument:
    if cst.data in ['variable', 'constructor', 'infix_operator', 'left_section', 'right_section', 'paren_term']:
        return TermArgument(Term_from_cst(cst))
    elif cst.data == 'type_instantiation':
        return TypeArgument(Type_from_cst(cst))
    raise


@dataclass
class TermArgument(ApplicationArgument):
    term: Term


@dataclass
class TypeArgument(ApplicationArgument):
    type: Type


# f {t} m
# ApplicationTerm(ApplicationTerm(f,TypeArgument(t)), TermArgument(m))
@dataclass
class ApplicationTerm(Term):
    function: Term
    argument: ApplicationArgument


@dataclass
class CaseClause(Surface):
    pattern: List[Pattern]
    guard: Guard
    body: Term


def CaseClause_from_cst(cst) -> CaseClause:
    return CaseClause([Pattern_from_cst(c) for c in cst.children[0].children],
                      Guard_from_cst(cst.children[1]),
                      Term_from_cst(cst.children[2]))


# case m | n of
#    | p | q || g -> r
#    ;;
# CaseTerm(
#   [m,n],
#   [
#     CaseClause(
#       [p,q],
#       g,
#       r
#     )
#   ]
# )
@dataclass
class CaseTerm(Term):
    scrutinees: List[Term]
    clauses: List[CaseClause]


@dataclass
class LetDeclaration(Surface):
    pass


def LetDeclaration_from_cst(cst) -> LetDeclaration:

    if cst.data == 'let_type_signature':
        return LetTypeSignature(name=cst.children[0].value,
                                type=Type_from_cst(cst.children[1]))
    elif cst.data == 'let_term_equation':
        return LetTermEquation(name=cst.children[0].value,
                               patterns=[Pattern_from_cst(pat)
                                         for pat in cst.children[1].children],
                               guard=Guard_from_cst(cst.children[2]),
                               definition=Term_from_cst(cst.children[3]))

    raise


@dataclass
class LetTypeSignature(LetDeclaration):
    name: str
    type: Type


@dataclass
class LetTermEquation(LetDeclaration):
    name: str
    patterns: List[Pattern]
    guard: Guard
    definition: Term


# let x : t
#   | x = m
# in n
# LetTerm(
#   [
#     LetTypeSignature("x",t),
#     LetTermEquation("x", [], Guard(), m)
#   ],
#   n
# )
@dataclass
class LetTerm(Term):
    declarations: List[LetDeclaration]
    scope: Term


# x * y
# InfixTerm("*", x, y)
@dataclass
class InfixTerm(Term):
    operator: str
    left_argument: Term
    right_argument: Term


# (*)
# InfixOperatorTerm("*")
@dataclass
class InfixOperatorTerm(Term):
    operator: str


# (x *)
# LeftSectionTerm("*", x)
@dataclass
class LeftSectionTerm(Term):
    operator: str
    left_argument: Term


# (* y)
# RightSectionTerm("*", y)
@dataclass
class RightSectionTerm(Term):
    operator: str
    right_argument: Term


################  Term Equation  ################


# x p q || g = m
# TermEquation("x", [p,q], g, m)
@dataclass
class TermEquation(Declaration):
    name: str
    patterns: List[Pattern]
    guard: Guard
    definition: Term


################  Instance Declaration  ################


@dataclass
class MethodEquation(Surface):
    name: str
    patterns: List[Pattern]
    guard: Guard
    definition: Term


def MethodEquation_from_cst(cst) -> MethodEquation:
    return MethodEquation(cst.children[0].value,
                          [Pattern_from_cst(pat)
                           for pat in cst.children[1].children],
                          Guard_from_cst(cst.children[2]),
                          Term_from_cst(cst.children[3]))


# instance forall C A where
#   | x = m
#   ;;
# InstanceDeclaration
@dataclass
class InstanceDeclaration(Declaration):
    instance_quantification: List[TyVarKinding]
    instance_precondition: List[ClassConstraint]
    name: str
    instance_parameters: List[Type]
    method_equations: List[MethodEquation]


################  Modules  ################


@dataclass
class ModuleName(Surface):
    root_name: str
    sub_names: List[str]


def ModuleName_from_cst(cst):
    return ModuleName(root_name=cst.children[0].value,
                      sub_names=[c.value for c in cst.children[1:]])


@dataclass
class ImportType(Surface):
    pass


def ImportType_from_cst(cst) -> ImportType:
    if cst.data == 'qualified_import':
        return QualifiedImportType(imported_name=ModuleName_from_cst(cst.children[0]))
    elif cst.data == 'unqualified_import':
        return UnqualifiedImportType(imported_name=ModuleName_from_cst(cst.children[0]),
                                     using_hiding=UsingHiding_from_cst(cst.children[1]))
    elif cst.data == 'as_import':
        return AsImportType(imported_name=cst.children[0].value,
                            as_name=cst.children[1].value)
    raise


@dataclass
class QualifiedImportType(ImportType):
    imported_name: str


@dataclass
class UsingHiding(Surface):
    pass


def UsingHiding_from_cst(cst) -> Optional[UsingHiding]:
    if len(cst.children) == 0:
        return None
    elif cst.children[0].data == 'import_using':
        return ImportUsing(names=[tok.value for tok in cst.children[0].children])
    elif cst.children[0].data == 'import_hiding':
        return ImportHiding(names=[tok.value for tok in cst.children[0].children])
    raise


@dataclass
class ImportUsing(UsingHiding):
    names: List[str]


@dataclass
class ImportHiding(UsingHiding):
    names: List[str]


@dataclass
class UnqualifiedImportType(ImportType):
    imported_name: str
    using_hiding: Optional[UsingHiding]


@dataclass
class AsImportType(ImportType):
    imported_name: str
    as_name: str


@dataclass
class RenamingClause(Surface):
    rename_from: str
    rename_to: str


def RenamingClause_from_cst(cst) -> RenamingClause:
    return RenamingClause(rename_from=cst.children[0].value,
                          rename_to=cst.children[1].value)


# import Foo
# import Foo renaming (a to b)
# import Foo as Bar
# import Foo unqualified
# import Foo unqualified using (a, b)
# import Foo unqualified hiding (a, b)
@dataclass
class Import(Surface):
    import_type: ImportType
    renaming: List[RenamingClause]


def Import_from_cst(cst) -> Import:
    return Import(import_type=ImportType_from_cst(cst.children[0]),
                  renaming=[RenamingClause_from_cst(cls)
                            for cls in cst.children[1].children])


# module M where ...
# module M importing ... where ...
@dataclass
class Module(Surface):
    imports: List[Import]
    declarations: List[Declaration]


def Module_from_cst(cst) -> Module:

    return Module(imports=[Import_from_cst(imp)
                           for imp in cst.children[0].children],
                  declarations=[Declaration_from_cst(decl)
                                for decl in cst.children[1].children])
