# This module defines the abstract syntax for Asteria's surface language. Very little of interest is happening in this file, except the definition of some types, plus the conversions from concrete syntax trees produced by Lark.

# The surface language has a numbr of user-friendly conveniences that make it nicer to program in Asteria. Among them are implicit instantiation of forall types, as well as implicit instance arguments. Beyond that, there are conveniences such as constructs as functions and multiple variables for any given binder.

from dataclasses import dataclass
from lark import Tree
from typing import List, Optional


@dataclass
class Syntax(object):
    pass

################  Kinds  ################


@dataclass
class Kind(Syntax):

    @staticmethod
    def from_cst(cst) -> Kind:
        if cst.data == 'type_kind':
            return TypeKind()
        elif cst.data == 'arrow_kind':
            return ArrowKind(Kind.from_cst(cst.children[0]), Kind.from_cst(cst.children[1]))
        elif cst.data == 'paren_kind':
            return Kind.from_cst(cst.children[0])
        raise


@dataclass
class TypeKind(Kind):
    pass


@dataclass
class ArrowKind(Kind):
    argument_kind: Kind
    return_kind: Kind


@dataclass
class TyVarKinding(Syntax):
    tyvar: List[str]
    kind: Kind

    @staticmethod
    def from_cst(cst) -> TyVarKinding:
        return TyVarKinding([tok.value for tok in cst.children[0].children], Kind.from_cst(cst.children[1]))


################  Types  ################

@dataclass
class Type(Syntax):

    @staticmethod
    def from_cst(cst) -> Type:

        if cst.data == 'type_variable':
            return VariableType(cst.children[0].value)
        elif cst.data == 'type_name':
            return NameType(cst.children[0].value)
        elif cst.data == 'function_type':
            return FunctionType(Type.from_cst(cst.children[0]), Type.from_cst(cst.children[1]))
        elif cst.data == 'constrained_type':
            return ConstrainedType([ClassConstraint.from_cst(con) for con in cst.children[0].children],
                                   Type.from_cst(cst.children[1]))
        elif cst.data == 'forall_type':
            return ForallType([TyVarKinding.from_cst(tvk) for tvk in cst.children[0].children],
                              Type.from_cst(cst.children[1]))
        elif cst.data == 'type_application':
            return ApplicationType(Type.from_cst(cst.children[0]), Type.from_cst(cst.children[1]))
        elif cst.data == 'paren_type':
            return Type.from_cst(cst.children[0])

        raise


@dataclass
class ClassConstraint(Syntax):
    name: str
    arguments: List[Type]

    @staticmethod
    def from_cst(cst) -> ClassConstraint:
        return ClassConstraint(cst.children[0].value,
                               [Type.from_cst(ty) for ty in cst.children[1].children])


@dataclass
class VariableType(Type):
    name: str


@dataclass
class NameType(Type):
    name: str


@dataclass
class FunctionType(Type):
    argument_type: Type
    return_type: Type


@dataclass
class ConstrainedType(Type):
    class_constraints: List[ClassConstraint]
    return_type: Type


@dataclass
class ForallType(Type):
    tyvar_kindings: List[TyVarKinding]
    scope: Type


@dataclass
class ApplicationType(Type):
    function: Type
    argument: Type


################  Declarations  ################

@dataclass
class Declaration(Syntax):

    @staticmethod
    def from_cst(cst) -> Declaration:

        if cst.data == 'fixity_declaration':
            return FixityDeclaration(fixity=cst.children[0].value,
                                     fixity_level=cst.children[1].value,
                                     operators=[op.value for op in cst.children[2].children])

        elif cst.data == 'data_declaration':
            return DataDeclaration(name=cst.children[0].value,
                                   tyvar_kindings=[TyVarKinding.from_cst(
                                       tvk) for tvk in cst.children[1].children],
                                   constructor_declarations=[ConstructorDeclaration.from_cst(cdecl) for cdecl in cst.children[2].children])

        elif cst.data == 'type_synonym_declaration':
            return TypeSynonymDeclaration(name=cst.children[0].value,
                                          tyvar_kindings=[TyVarKinding.from_cst(
                                              tvk) for tvk in cst.children[1].children],
                                          definition=Type.from_cst(cst.children[2]))
        elif cst.data == 'type_class_declaration':
            return TypeClassDeclaration(class_quantification=[TyVarKinding.from_cst(tvk) for tvk in cst.children[0].children[0].children],
                                        class_precondition=[ClassConstraint.from_cst(
                                            con) for con in cst.children[1].children],
                                        name=cst.children[2].value,
                                        class_params=[
                                            tok.value for tok in cst.children[3].children],
                                        method_declarations=[MethodDeclaration.from_cst(mdecl) for mdecl in cst.children[4].children])
        elif cst.data == 'type_signature':
            return TypeSignature(name=cst.children[0].value,
                                 type=Type.from_cst(cst.children[1]))
        elif cst.data == 'term_equation':
            return TermEquation(name=cst.children[0].value,
                                patterns=[Pattern.from_cst(pat)
                                          for pat in cst.children[1].children],
                                guard=Guard.from_cst(cst.children[2]),
                                definition=Term.from_cst(cst.children[3]))
        elif cst.data == 'instance_declaration':
            return InstanceDeclaration([TyVarKinding.from_cst(tvk) for tvk in cst.children[0].children[0].children],
                                       [ClassConstraint.from_cst(
                                           con) for con in cst.children[1].children],
                                       cst.children[2].value,
                                       [Type.from_cst(ty)
                                        for ty in cst.children[3].children],
                                       [MethodEquation.from_cst(meq) for meq in cst.children[4].children])

        raise

@dataclass
class ImplicitTyVarKinding(Syntax):
    tyvar: str
    kind: Kind

    @staticmethod
    def from_cst(cst) -> ImplicitTyVarKinding:
        return ImplicitTyVarKinding(cst.children[0].value, Kind.from_cst(cst.children[1]))


@dataclass
class VarTyping(Syntax):
    var: List[str]
    type: Type

    @staticmethod
    def from_cst(cst) -> VarTyping:
        return VarTyping([tok.value for tok in cst.children[0].children], Type.from_cst(cst.children[1]))


################  Fixity Declarations  ################

@dataclass
class Fixity(Syntax):

    @staticmethod
    def from_cst(cst) -> Fixity:
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


@dataclass
class FixityDeclaration(Declaration):
    fixity: Fixity
    fixity_level: int
    operators: List[str]


################  Data Declarations  ################


@dataclass
class ConstructorDeclaration(Syntax):
    name: str
    tyvar_kindings: List[TyVarKinding]
    var_typings: List[VarTyping]
    return_type: Type

    @staticmethod
    def from_cst(cst) -> ConstructorDeclaration:
        return ConstructorDeclaration(cst.children[0].value,
                                      [TyVarKinding.from_cst(
                                          tvk) for tvk in cst.children[1].children],
                                      [VarTyping.from_cst(
                                          vt) for vt in cst.children[2].children],
                                      Type.from_cst(cst.children[3]))


@dataclass
class DataDeclaration(Declaration):
    name: str
    tyvar_kindings: List[TyVarKinding]
    constructor_declarations: List[ConstructorDeclaration]


################  Type Synonym Declarations  ################

@dataclass
class TypeSynonymDeclaration(Declaration):
    name: str
    tyvar_kindings: List[TyVarKinding]
    definition: Type


################  Type Class Declarations  ################

@dataclass
class MethodDeclaration(Syntax):
    name: str
    type: Type

    @staticmethod
    def from_cst(cst) -> MethodDeclaration:
        return MethodDeclaration(cst.children[0].value,
                                 Type.from_cst(cst.children[1]))


@dataclass
class TypeClassDeclaration(Declaration):
    class_quantification: List[TyVarKinding]
    class_precondition: List[ClassConstraint]
    name: str
    class_params: List[str]
    method_declarations: List[MethodDeclaration]


################  Type Signature  ################

@dataclass
class TypeSignature(Declaration):
    name: str
    type: Type


################  Patterns  ################

@dataclass
class Pattern(Syntax):

    @staticmethod
    def from_cst(cst) -> Pattern:
        if cst.data == 'variable_pattern':
            return VariablePattern(cst.children[0].value)
        elif cst.data == 'constructor_pattern':
            return ConstructorPattern(cst.children[0].value,
                                      [Pattern.from_cst(pat) for pat in cst.children[1].children])
        elif cst.data == 'paren_pattern':
            return Pattern.from_cst(cst.children[0])
        raise


@dataclass
class VariablePattern(Pattern):
    name: str


@dataclass
class ConstructorPattern(Pattern):
    constructor: str
    arguments: List[Pattern]


################  Terms  ################

@dataclass
class Term(Syntax):

    @staticmethod
    def from_cst(cst) -> Term:
        if cst.data == 'type_annotation':
            return TypeAnnotationTerm(Term.from_cst(cst.children[0]),
                                      Type.from_cst(cst.children[1]))
        elif cst.data == 'variable':
            return VariableTerm(cst.children[0].value)
        elif cst.data == 'constructor':
            return ConstructorTerm(cst.children[0].value)
        elif cst.data == 'lambda':
            return LambdaTerm([FormalParameter.from_cst(par) for par in cst.children[0].children],
                              Term.from_cst(cst.children[1]))
        elif cst.data == 'application':
            return ApplicationTerm(Term.from_cst(cst.children[0]),
                                   ApplicationArgument.from_cst(cst.children[1]))
        elif cst.data == 'case':
            return CaseTerm(Term.from_cst(cst.children[0]),
                            [CaseClause.from_cst(cls) for cls in cst.children[1].children])
        elif cst.data == 'let':
            return LetTerm([LetDeclaration.from_cst(decl) for decl in cst.children[0].children],
                           Term.from_cst(cst.children[1]))
        elif cst.data == 'infix':
            return InfixTerm(cst.children[1].value,
                             Term.from_cst(cst.children[0]),
                             Term.from_cst(cst.children[2]))
        elif cst.data == 'infix_operator':
            return InfixOperatorTerm(cst.children[0].value)
        elif cst.data == 'left_slice':
            return LeftSliceTerm(cst.children[1].value,
                                 Term.from_cst(cst.children[0]))
        elif cst.data == 'right_slice':
            return RightSliceTerm(cst.children[0].value,
                                  Term.from_cst(cst.children[1]))
        elif cst.data == 'paren_term':
            return Term.from_cst(cst.children[0])
        raise


@dataclass
class Guard(Syntax):
    term: Optional[Term]

    @staticmethod
    def from_cst(cst) -> Guard:
        if cst.children[0] is None:
            return Guard(None)
        else:
            return Guard(Term.from_cst(cst.children[0]))


@dataclass
class TypeAnnotationTerm(Term):
    term: Term
    type: Type


@dataclass
class VariableTerm(Term):
    name: str


@dataclass
class ConstructorTerm(Term):
    constructor: str


@dataclass
class LambdaTerm(Term):
    formal_parameters: List[FormalParameter]
    body: Term


@dataclass
class ApplicationTerm(Term):
    function: Term
    argument: ApplicationArgument


@dataclass
class CaseClause(Syntax):
    pattern: Pattern
    guard: Guard
    body: Term

    @staticmethod
    def from_cst(cst) -> CaseClause:
        return CaseClause(Pattern.from_cst(cst.children[0]),
                          Guard.from_cst(cst.children[1]),
                          Term.from_cst(cst.children[2]))


@dataclass
class CaseTerm(Term):
    scrutinee: Term
    clauses: List[CaseClause]


@dataclass
class LetDeclaration(Syntax):

    @staticmethod
    def from_cst(cst) -> LetDeclaration:

        if cst.data == 'let_type_signature':
            return LetTypeSignature(name=cst.children[0].value,
                                    type=Type.from_cst(cst.children[1]))
        elif cst.data == 'let_term_equation':
            return LetTermEquation(name=cst.children[0].value,
                                   patterns=[Pattern.from_cst(pat)
                                             for pat in cst.children[1].children],
                                   guard=Guard.from_cst(cst.children[2]),
                                   definition=Term.from_cst(cst.children[3]))

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



@dataclass
class LetTerm(Term):
    declarations: List[LetDeclaration]
    scope: Term


@dataclass
class InfixTerm(Term):
    operator: str
    left_argument: Term
    right_argument: Term


@dataclass
class InfixOperatorTerm(Term):
    operator: str


@dataclass
class LeftSliceTerm(Term):
    operator: str
    argument: Term


@dataclass
class RightSliceTerm(Term):
    operator: str
    argument: Term


@dataclass
class ApplicationArgument(Syntax):

    @staticmethod
    def from_cst(cst) -> ApplicationArgument:
        if cst.data in ['variable', 'constructor', 'infix_operator', 'left_slice', 'right_slice', 'paren_term']:
            return TermArgument(Term.from_cst(cst))
        elif cst.data == 'type_instantiation':
            return TypeArgument(Type.from_cst(cst))
        raise


@dataclass
class TermArgument(ApplicationArgument):
    term: Term


@dataclass
class TypeArgument(ApplicationArgument):
    type: Type


@dataclass
class FormalParameter(Syntax):

    @staticmethod
    def from_cst(cst) -> FormalParameter:
        if cst.data == 'variable_parameter':
            return UntypedVariableParameter(cst.children[0].value)
        elif cst.data == 'vartyping':
            return TypedVariableParameter(cst.children[0].value,
                                          Type.from_cst(cst.children[1]))
        elif cst.data == 'type_variable_parameter':
            return UnkindedTyVarParameter(cst.children[0].value)
        elif cst.data == 'implicit_tyvarkinding':
            return KindedTyVarParameter(cst.children[0].value,
                                        Kind.from_cst(cst.children[1]))
        raise


@dataclass
class UntypedVariableParameter(FormalParameter):
    name: str


@dataclass
class TypedVariableParameter(FormalParameter):
    name: str
    type: Type


@dataclass
class UnkindedTyVarParameter(FormalParameter):
    name: str


@dataclass
class KindedTyVarParameter(FormalParameter):
    name: str
    kind: Kind


################  Term Equation  ################


@dataclass
class TermEquation(Declaration):
    name: str
    patterns: List[Pattern]
    guard: Guard
    definition: Term


################  Instance Declaration  ################


@dataclass
class MethodEquation(Syntax):
    name: str
    patterns: List[Pattern]
    guard: Guard
    definition: Term

    @staticmethod
    def from_cst(cst) -> MethodEquation:
        return MethodEquation(cst.children[0].value,
                              [Pattern.from_cst(pat)
                               for pat in cst.children[1].children],
                              Guard.from_cst(cst.children[2]),
                              Term.from_cst(cst.children[3]))


@dataclass
class InstanceDeclaration(Declaration):
    class_quantification: List[TyVarKinding]
    class_precondition: List[ClassConstraint]
    name: str
    arguments: List[Type]
    method_equations: List[MethodEquation]


################  Modules  ################


@dataclass
class ImportType(Syntax):

    @staticmethod
    def from_cst(cst) -> ImportType:
        if cst.data == 'qualified_import':
            return QualifiedImportType(imported_name=cst.children[0].value)
        elif cst.data == 'unqualified_import':
            return UnqualifiedImportType(imported_name=cst.children[0].value,
                                         using_hiding=UsingHiding.from_cst(cst.children[1]))
        elif cst.data == 'as_import':
            return AsImportType(imported_name=cst.children[0].value,
                                as_name=cst.children[1].value)
        raise


@dataclass
class QualifiedImportType(ImportType):
    imported_name: str


@dataclass
class UnqualifiedImportType(ImportType):
    imported_name: str
    using_hiding: Optional[UsingHiding]


@dataclass
class AsImportType(ImportType):
    imported_name: str
    as_name: str


@dataclass
class UsingHiding(Syntax):

    @staticmethod
    def from_cst(cst) -> Optional[UsingHiding]:
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
class RenamingClause(Syntax):
    rename_from: str
    rename_to: str

    @staticmethod
    def from_cst(cst) -> RenamingClause:
        return RenamingClause(rename_from=cst.children[0].value,
                              rename_to=cst.children[1].value)


@dataclass
class Import(Syntax):
    import_type: ImportType
    renaming: List[RenamingClause]

    @staticmethod
    def from_cst(cst) -> Import:
        return Import(import_type=ImportType.from_cst(cst.children[0]),
                      renaming=[RenamingClause.from_cst(cls)
                                for cls in cst.children[1].children])


@dataclass
class Module(Syntax):
    name: str
    imports: List[Import]
    declarations: List[Declaration]

    @staticmethod
    def from_cst(cst) -> Module:

        return Module(name=cst.children[0].value,
                      imports=[Import.from_cst(imp)
                               for imp in cst.children[1].children],
                      declarations=[Declaration.from_cst(decl)
                                    for decl in cst.children[2].children])
