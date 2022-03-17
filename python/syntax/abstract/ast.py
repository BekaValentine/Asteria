class Syntax(object):

    def __init__(self, *args, **kwargs):

        if len(args) + len(kwargs) != len(self.__class__.__slots__):
            raise TypeError(self.__class__.__name__ +
                            ' takes ' +
                            str(len(self.__class__.__slots__)) +
                            ' arguments but was given ' +
                            str(len(args)))

        remaining_slots = [*self.__class__.__slots__]

        for k, v in kwargs.items():
            if k not in self.__class__.__slots__:
                raise TypeError(self.__class__.__name__ +
                                ' has no slot named ' +
                                k +
                                '. Possible slots: ' +
                                ', '.join(self.__class__.__slots__))

            remaining_slots.remove(k)
            self.__dict__[k] = v

        for i, v in enumerate(args):
            self.__dict__[remaining_slots[i]] = v

    def __repr__(self):
        return self.__class__.__name__ + '(' + ','.join([repr(value) for _, value in self.__dict__.items()]) + ')'


################  Modules  ################


class Module(Syntax):
    __slots__ = ['name', 'imports', 'declarations']

    @staticmethod
    def from_cst(cst):

        return Module(name=cst.children[0].value,
                      imports=[Import.from_cst(imp)
                               for imp in cst.children[1].children],
                      declarations=[Declaration.from_cst(decl)
                                    for decl in cst.children[2].children])


class Import(Syntax):
    __slots__ = ['import_type', 'renaming']

    @staticmethod
    def from_cst(cst):
        return Import(import_type=ImportType.from_cst(cst.children[0]),
                      renaming=[RenamingClause.from_cst(cls)
                                for cls in cst.children[1].children])


class ImportType(Syntax):

    @staticmethod
    def from_cst(cst):

        if cst.data == 'qualified_import':
            return QualifiedImportType(imported_name=cst.children[0].value)
        elif cst.data == 'unqualified_import':
            return UnqualifiedImportType(imported_name=cst.children[0].value,
                                         using_hiding=UsingHiding.from_cst(cst.children[1]))
        elif cst.data == 'as_import':
            return AsImportType(imported_name=cst.children[0].value,
                                as_name=cst.children[1].value)


class QualifiedImportType(ImportType):
    __slots__ = ['imported_name']


class UnqualifiedImportType(ImportType):
    __slots__ = ['imported_name', 'using_hiding']


class AsImportType(ImportType):
    __slots__ = ['imported_name', 'as_name']


class UsingHiding(Syntax):

    @staticmethod
    def from_cst(cst):

        if len(cst.children) == 0:
            return None
        elif cst.children[0].data == 'import_using':
            return ImportUsing(names=[tok.value for tok in cst.children[0].children])
        elif cst.children[0].data == 'import_hiding':
            return ImportHiding(names=[tok.value for tok in cst.children[0].children])


class ImportUsing(UsingHiding):
    __slots__ = ['names']


class ImportHiding(UsingHiding):
    __slots__ = ['names']


class RenamingClause(Syntax):
    __slots__ = ['rename_from', 'rename_to']

    @staticmethod
    def from_cst(cst):
        return RenamingClause(rename_from=cst.children[0].value,
                              rename_to=cst.children[1].value)


################  Declarations  ################

class Declaration(Syntax):

    @staticmethod
    def from_cst(cst):

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


class TyVarKinding(Syntax):
    __slots__ = ['tyvar', 'kind']

    @staticmethod
    def from_cst(cst):
        return TyVarKinding([tok.value for tok in cst.children[0].children], Kind.from_cst(cst.children[1]))


class ImplicitTyVarKinding(Syntax):
    __slots__ = ['tyvar', 'kind']

    @staticmethod
    def from_cst(cst):
        return ImplicitTyVarKinding(cst.children[0].value, Kind.from_cst(cst.children[1]))


class VarTyping(Syntax):
    __slots__ = ['var', 'type']

    @staticmethod
    def from_cst(cst):
        return VarTyping([tok.value for tok in cst.children[0].children], Type.from_cst(cst.children[1]))


################  Fixity Declarations  ################

class FixityDeclaration(Declaration):
    __slots__ = ['fixity', 'fixity_level', 'operators']


################  Data Declarations  ################


class DataDeclaration(Declaration):
    __slots__ = ['name', 'tyvar_kindings', 'constructor_declarations']


class ConstructorDeclaration(Syntax):
    __slots__ = ['name', 'tyvar_kindings', 'var_typings', 'return_type']

    @staticmethod
    def from_cst(cst):
        return ConstructorDeclaration(cst.children[0].value,
                                      [TyVarKinding.from_cst(
                                          tvk) for tvk in cst.children[1].children],
                                      [VarTyping.from_cst(
                                          vt) for vt in cst.children[2].children],
                                      Type.from_cst(cst.children[3]))


################  Type Synonym Declarations  ################

class TypeSynonymDeclaration(Declaration):
    __slots__ = ['name', 'tyvar_kindings', 'definition']


################  Type Class Declarations  ################

class TypeClassDeclaration(Declaration):
    __slots__ = ['class_quantification', 'class_precondition',
                 'name', 'class_params', 'method_declarations']


class MethodDeclaration(Syntax):
    __slots__ = ['name', 'type']

    @staticmethod
    def from_cst(cst):
        return MethodDeclaration(cst.children[0].value,
                                 Type.from_cst(cst.children[1]))


################  Type Signature  ################

class TypeSignature(Declaration):
    __slots__ = ['name', 'type']


################  Term Equation  ################

class TermEquation(Declaration):
    __slots__ = ['name', 'patterns', 'guard', 'definition']


class Guard(Syntax):
    __slots__ = ['term']

    @staticmethod
    def from_cst(cst):
        if len(cst.children) == 0:
            return Guard(None)
        else:
            return Guard(Term.from_cst(cst.children[0]))


################  Instance Declaration  ################


class InstanceDeclaration(Declaration):
    __slots__ = ['class_quantification', 'class_precondition',
                 'name', 'arguments', 'method_equations']


class MethodEquation(Syntax):
    __slots__ = ['name', 'patterns', 'guard', 'definition']

    @staticmethod
    def from_cst(cst):
        return MethodEquation(cst.children[0].value,
                              [Pattern.from_cst(pat)
                               for pat in cst.children[1].children],
                              Guard.from_cst(cst.children[2]),
                              Term.from_cst(cst.children[3]))


################  Kinds  ################


class Kind(Syntax):

    @staticmethod
    def from_cst(cst):
        if cst.data == 'type_kind':
            return TypeKind()
        elif cst.data == 'arrow_kind':
            return ArrowKind(Kind.from_cst(cst.children[0]), Kind.from_cst(cst.children[1]))
        elif cst.data == 'paren_kind':
            return Kind.from_cst(cst.children[0])


class TypeKind(Kind):
    __slots__ = []


class ArrowKind(Kind):
    __slots__ = ['argument_kind', 'return_kind']


################  Types  ################

class Type(Syntax):

    @staticmethod
    def from_cst(cst):

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


class VariableType(Type):
    __slots__ = ['name']


class NameType(Type):
    __slots__ = ['name']


class FunctionType(Type):
    __slots__ = ['argument_type', 'return_type']


class ConstrainedType(Type):
    __slots__ = ['class_constraints', 'return_type']


class ForallType(Type):
    __slots__ = ['tyvar_kindings', 'scope']


class ApplicationType(Type):
    __slots__ = ['function', 'argument']


################  Class Constraints  ################

class ClassConstraint(Syntax):
    __slots__ = ['name', 'arguments']

    @staticmethod
    def from_cst(cst):
        return ClassConstraint(cst.children[0].value,
                               [Type.from_cst(ty) for ty in cst.children[1].children])


################  Patterns  ################

class Pattern(Syntax):

    @staticmethod
    def from_cst(cst):

        if cst.data == 'variable_pattern':
            return VariablePattern(cst.children[0].value)
        elif cst.data == 'constructor_pattern':
            return ConstructorPattern(cst.children[0].value,
                                      [Pattern.from_cst(pat) for pat in cst.children[1].children])
        elif cst.data == 'paren_pattern':
            return Pattern.from_cst(cst.children[0])


class VariablePattern(Pattern):
    __slots__ = ['name']


class ConstructorPattern(Pattern):
    __slots__ = ['constructor', 'arguments']


################  Terms  ################

class Term(Syntax):

    @staticmethod
    def from_cst(cst):

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


class TypeAnnotationTerm(Term):
    __slots__ = ['term', 'type']


class VariableTerm(Term):
    __slots__ = ['name']


class ConstructorTerm(Term):
    __slots__ = ['constructor']


class LambdaTerm(Term):
    __slots__ = ['formal_parameters', 'body']


class ApplicationTerm(Term):
    __slots__ = ['function', 'argument']


class CaseTerm(Term):
    __slots__ = ['scrutinee', 'clauses']


class LetTerm(Term):
    __slots__ = ['declarations', 'scope']


class InfixTerm(Term):
    __slots__ = ['operator', 'left_argument', 'right_argument']


class InfixOperatorTerm(Term):
    __slots__ = ['operator']


class LeftSliceTerm(Term):
    __slots__ = ['operator', 'argument']


class RightSliceTerm(Term):
    __slots__ = ['operator', 'argument']


class ApplicationArgument(Syntax):

    @staticmethod
    def from_cst(cst):

        if cst.data in ['variable', 'constructor', 'infix_operator', 'left_slice', 'right_slice', 'paren_term']:
            return TermArgument(Term.from_cst(cst))
        elif cst.data == 'type_instantiation':
            return TypeArgument(Type.from_cst(cst))


class TermArgument(ApplicationArgument):
    __slots__ = ['term']


class TypeArgument(ApplicationArgument):
    __slots__ = ['type']


class CaseClause(Syntax):
    __slots__ = ['pattern', 'guard', 'body']

    @staticmethod
    def from_cst(cst):
        return CaseClause(Pattern.from_cst(cst.children[0]),
                          Guard.from_cst(cst.children[1]),
                          Term.from_cst(cst.children[2]))


class FormalParameter(Syntax):

    @staticmethod
    def from_cst(cst):

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


class UntypedVariableParameter(FormalParameter):
    __slots__ = ['name']


class TypedVariableParameter(FormalParameter):
    __slots__ = ['name', 'type']


class UnkindedTyVarParameter(FormalParameter):
    __slots__ = ['name']


class KindedTyVarParameter(FormalParameter):
    __slots__ = ['name', 'kind']
