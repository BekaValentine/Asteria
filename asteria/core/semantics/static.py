from dataclasses import dataclass
from typing import List, Dict, Optional, Tuple, cast

from asteria.utils import *
from asteria.core.syntax.abstract import *
import asteria.core.semantics.errors as errors
from asteria.core.semantics.contexts import *
from asteria.core.semantics.tasks import *


@dataclass
class ElaboratorState(object):
    type_constructor_signatures: Dict[DeclaredTypeConstructorName,
                                      TypeConstructorSignature]
    type_constructor_constructors: Dict[DeclaredTypeConstructorName,
                                        Dict[str, Scope[ConstructorSignature]]]
    term_signatures: Dict[DeclaredTermName, Type]
    term_definitions: Dict[DeclaredTermName, Term]

    task_stack: List[ElaboratorTask]


class Elaborator(object):
    elaborator_state: ElaboratorState

    verbose: bool

    def __init__(self):
        self.elaborator_state = ElaboratorState(
            type_constructor_signatures={},
            type_constructor_constructors={},
            term_signatures={},
            term_definitions={},
            task_stack=[])

        self.verbose = False

    def log(self, msg: str) -> None:
        if self.verbose:
            print(msg)

    def elab_module(self, name: List[str], mod: Module):
        self.elaborator_state.task_stack.append(ElabModule(name, mod))

        for decl in mod.declarations:
            if isinstance(decl, DataDeclaration):
                self.elab_data_decl(name, decl)
            elif isinstance(decl, TermDeclaration):
                self.elab_term_decl(name, decl)

        self.elaborator_state.task_stack.pop()

    def elab_data_decl(self, mods: List[str], decl: DataDeclaration):
        self.elaborator_state.task_stack.append(ElabDataDeclaration(decl))

        tycon = DeclaredTypeConstructorName(
            modules=mods,
            name=decl.name)

        if tycon in self.elaborator_state.type_constructor_signatures:
            raise errors.TypeConstructorAlreadyDefinedError(
                tasks=self.elaborator_state.task_stack,
                declaration=decl)

        params = [tvk.tyvar for tvk in decl.signature.parameters]
        old = []
        for param in params:
            if param not in old:
                old.append(param)
            else:
                raise errors.RepeatedParameterInTypeConstructorDeclarationError(
                    tasks=self.elaborator_state.task_stack,
                    parameter_name=param,
                    declaration=decl)

        self.log(
            f'Type constructor {tycon.pretty()} {decl.signature.pretty()}')

        self.elaborator_state.type_constructor_signatures[tycon] = decl.signature
        self.elaborator_state.type_constructor_constructors[tycon] = {}

        for condecl in decl.constructors:
            self.elab_condecl(tycon, decl.signature, condecl)

        self.elaborator_state.task_stack.pop()

    def elab_condecl(self, tycon: DeclaredTypeConstructorName, tycon_sig: TypeConstructorSignature, condecl: ConstructorDeclaration) -> bool:
        self.elaborator_state.task_stack.append(
            ElabConstructorDeclaration(condecl))

        if condecl.name in self.elaborator_state.type_constructor_constructors[tycon]:
            raise errors.TermConstructorAlreadyDefinedError(
                tasks=self.elaborator_state.task_stack,
                declaration=condecl)

        new_tycon_param_names, tycon_level_sig = condecl.signature.open([])

        ctx = Context({typaram: HasKind(tvk.kind)
                       for typaram, tvk in zip(new_tycon_param_names, tycon_sig.parameters)})

        previous_names = []

        for type_parameter in tycon_level_sig.term_signature.names:
            if type_parameter in previous_names:
                raise errors.RepeatedTypeParameterInTermConstructorDeclarationError(
                    tasks=self.elaborator_state.task_stack,
                    type_parameter_name=type_parameter,
                    declaration=condecl)
            previous_names.append(type_parameter)

        new_typaram_names, termsig = tycon_level_sig.term_signature.open(
            ctx.names())

        ctx = ctx.extend({
            v: HasKind(k)
            for v, k in zip(new_typaram_names, tycon_level_sig.type_parameters_kinds)
        })

        for param, type in termsig.parameters:
            if param in previous_names:
                raise errors.RepeatedTermParameterInTermConstructorDeclarationError(
                    tasks=self.elaborator_state.task_stack,
                    term_parameter_name=param,
                    declaration=condecl)
            previous_names.append(param)
            k = self.synth_type(ctx, type)
            if not isinstance(k, TypeKind):
                raise errors.IncorrectKindForConstructorParameterTypeError(
                    tasks=self.elaborator_state.task_stack,
                    parameter_name=param,
                    found_kind=k,
                    declaration=condecl)

        ctx = ctx.extend({v: HasType(t) for v, t in termsig.parameters})

        k = self.synth_type(ctx, termsig.return_type)
        if not isinstance(k, TypeKind):
            raise errors.IncorrectKindForConstructorReturnTypeError(
                tasks=self.elaborator_state.task_stack,
                found_kind=k,
                declaration=condecl)

        if not isinstance(termsig.return_type, ConstructorType) or termsig.return_type.name != tycon:
            raise errors.IncorrectTypeForConstructorDeclarationReturnTypeError(
                tasks=self.elaborator_state.task_stack,
                found_type=termsig.return_type,
                type_constructor=tycon,
                declaration=condecl)

        self.log(
            f'Term constructor {DeclaredConstructorName(tycon,condecl.name).pretty()} : {tycon_level_sig.pretty()}')
        self.elaborator_state.type_constructor_constructors[tycon][condecl.name] = condecl.signature

        self.elaborator_state.task_stack.pop()

        return True

    def elab_term_decl(self, mods: List[str], termdecl: TermDeclaration):

        self.elaborator_state.task_stack.append(ElabTermDeclaration(termdecl))

        name = DeclaredTermName(mods, termdecl.name)
        if name in self.elaborator_state.term_signatures:
            raise errors.TermNameAlreadyDefinedError(
                tasks=self.elaborator_state.task_stack,
                declaration=termdecl)

        k = self.synth_type(Context({}), termdecl.type)
        if not isinstance(k, TypeKind):
            raise errors.IncorrectKindForTermDeclarationTypeError(
                tasks=self.elaborator_state.task_stack,
                found_kind=k,
                declaration=termdecl)

        self.log(
            f'Term name {name.pretty()} : {termdecl.type.pretty()}')
        self.elaborator_state.term_signatures[name] = termdecl.type

        self.check_term(Context({}), termdecl.type, termdecl.definition)

        self.elaborator_state.term_definitions[name] = termdecl.definition

        self.elaborator_state.task_stack.pop()

    def synth_type(self, ctx: Context, t: Type) -> Kind:
        self.elaborator_state.task_stack.append(SynthType(ctx, t))

        ret_kind: Kind

        if isinstance(t, VariableType):
            if t.name not in ctx.variables:
                raise errors.TypeVariableNotInScopeError(
                    tasks=self.elaborator_state.task_stack,
                    type=t,
                    context=ctx)
            elif isinstance(ctx.variables[t.name], HasType):
                raise errors.TermVariableUsedAsTypeVariableError(
                    tasks=self.elaborator_state.task_stack,
                    type=t,
                    context=ctx)

            ret_kind = cast(HasKind, ctx.variables[t.name]).kind
        elif isinstance(t, ConstructorType):
            if t.name not in self.elaborator_state.type_constructor_signatures:
                raise errors.TypeConstructorNotInScopeError(
                    tasks=self.elaborator_state.task_stack,
                    type=t,
                    known_type_constructors=self.elaborator_state.type_constructor_signatures)
            sig = [
                tvk.kind for tvk in self.elaborator_state.type_constructor_signatures[t.name].parameters]
            for i, arg in enumerate(t.arguments):
                k = self.synth_type(ctx, arg)
                if k != sig[i]:
                    raise errors.IncorrectKindForTypeParameterError(
                        tasks=self.elaborator_state.task_stack,
                        type=t,
                        parameter=arg,
                        expected_kind=sig[i],
                        found_kind=k)

            ret_kind = TypeKind(source=None)
        elif isinstance(t, FunctionType):
            k = self.synth_type(ctx, t.argument_type)
            if not isinstance(k, TypeKind):
                raise errors.IncorrectKindForFunctionTypeArgumentTypeError(
                    tasks=self.elaborator_state.task_stack,
                    type=t,
                    found_kind=k)

            k = self.synth_type(ctx, t.return_type)
            if not isinstance(k, TypeKind):
                raise errors.IncorrectKindForFunctionTypeReturnTypeError(
                    tasks=self.elaborator_state.task_stack,
                    type=t,
                    found_kind=k)

            ret_kind = TypeKind(source=None)
        elif isinstance(t, ForallType):
            ([new_var], new_body) = t.scope.open(ctx.names())
            ctx2 = ctx.extend({new_var: HasKind(t.tyvar_kind)})
            k = self.synth_type(ctx2, new_body)
            if not isinstance(k, TypeKind):
                raise errors.IncorrectKindForForallTypeScopeError(
                    tasks=self.elaborator_state.task_stack,
                    type=t,
                    found_kind=k)

            ret_kind = TypeKind(source=None)
        elif isinstance(t, LambdaType):
            ([new_var], new_body) = t.body.open(ctx.names())
            ctx2 = ctx.extend({new_var: HasKind(t.tyvar_kind)})
            k = self.synth_type(ctx2, new_body)

            ret_kind = FunctionKind(
                source=None,
                argument_kind=t.tyvar_kind, return_kind=k)
        elif isinstance(t, ApplicationType):
            k = self.synth_type(ctx, t.function)

            if not isinstance(k, FunctionKind):
                raise errors.IncorrectKindForApplicationTypeFunctionError(
                    tasks=self.elaborator_state.task_stack,
                    type=t,
                    found_kind=k)

            k2 = self.synth_type(ctx, t.argument)
            if k.argument_kind != k2:
                raise errors.IncorrectKindForApplicationTypeArgumentError(
                    tasks=self.elaborator_state.task_stack,
                    type=t,
                    expected_kind=k.argument_kind,
                    found_kind=k2)

            ret_kind = k.return_kind
        else:
            todo()
            raise

        self.elaborator_state.task_stack.pop()

        return ret_kind

    def check_term(self, ctx: Context, t: Type, m: Term) -> None:
        self.elaborator_state.task_stack.append(CheckTerm(ctx, t, m))

        if isinstance(t, FunctionType) and isinstance(m, LambdaTerm):
            ([new_var], new_body) = m.body.open(ctx.names())
            self.check_term(ctx.extend(
                {new_var: HasType(t.argument_type)}), t.return_type, new_body)
        elif isinstance(t, ForallType) and isinstance(m, AbstractionTerm):
            ([new_var], new_type_scope) = t.scope.open(ctx.names())
            new_body = m.body.body.rename({m.body.names[0]: new_var})
            self.check_term(ctx.extend(
                {new_var: HasKind(t.tyvar_kind)}), new_type_scope, new_body)

        elif isinstance(t, ConstructorType) and isinstance(m, ConstructorTerm):
            if m.constructor not in self.elaborator_state.type_constructor_constructors[t.name]:
                raise errors.TermConstructorNotInScopeError(
                    tasks=self.elaborator_state.task_stack,
                    term=m,
                    type=t,
                    known_term_constructors=self.elaborator_state.type_constructor_constructors[t.name])

            new_tycon_param_names, consig = self.elaborator_state.type_constructor_constructors[t.name][m.constructor].open(
                ctx.names())

            ctx = ctx.extend({
                v: HasTypeValue(t)
                for v, t in zip(new_tycon_param_names, t.arguments)
            })

            if len(m.type_arguments) != len(consig.type_parameters_kinds):
                raise errors.IncorrectNumberOfTypeArgumentsForConstructor(
                    tasks=self.elaborator_state.task_stack,
                    term=m,
                    expected_number=len(consig.type_parameters_kinds),
                    found_number=len(m.type_arguments))

            new_typaram_names, termsig = consig.term_signature.open(
                ctx.names())

            for tyargkind, tyarg in zip(consig.type_parameters_kinds, m.type_arguments):
                k = self.synth_type(ctx, tyarg)
                if tyargkind != k:
                    print(
                        f'ERROR: Incorrect kind for type argument {tyarg.pretty()} in constructor term {m.pretty()}')
                    print(f'ERROR: Expected kind: {tyargkind.pretty()}')
                    print(f'ERROR: Found kind: {k.pretty()}')
                    exit()

            ctx = ctx.extend({
                tyvar: HasTypeValue(tyval)
                for tyvar, tyval in zip(new_typaram_names, m.type_arguments)
            })

            if len(m.arguments) != len(termsig.parameters):
                print(
                    f'ERROR: Incorrect number of arguments in constructor term {m.pretty()}')
                print(
                    f'Expected {len(termsig.parameters)}')
                print(f'Found {len(m.arguments)}')
                exit()

            for (_, argty), arg in zip(termsig.parameters, m.arguments):
                self.check_term(ctx, eval_type(
                    ctx, argty), arg)

            found_ty = eval_type(ctx, termsig.return_type)
            if found_ty != t:
                print(
                    f'ERROR: Constructor term does not inhabit given type: {m.pretty()}')
                print(f'ERROR: Expected type: {t.pretty()}')
                print(f'ERROR: Actual type: {found_ty.pretty()}')
                exit()
        else:
            t2 = self.synth_term(ctx, m)
            if t2 != t:
                print(
                    f'ERROR: Term with synthesized type does not inhabit expected type')
                print(f'ERROR: Term: {m.pretty()}')
                print(f'ERROR: Expected type: {t.pretty()}')
                print(f'ERROR: Synthesized type: {t2.pretty()}')
                exit()

        self.elaborator_state.task_stack.pop()

    def synth_term(self, ctx: Context, m: Term) -> Type:

        self.elaborator_state.task_stack.append(SynthTerm(ctx, m))

        ret_type: Type

        if isinstance(m, VariableTerm):
            if m.name not in ctx.variables:
                print(f'ERROR: Found an unbound variable: {m.pretty()}')
                exit()
            if not isinstance(ctx.variables[m.name], HasType):
                print(
                    f'ERROR: Variable {m.pretty()} is used as a term variable but is declared as a type variable.')
                exit()
            ret_type = cast(HasType, ctx.variables[m.name]).type
        elif isinstance(m, DeclaredTermNameTerm):
            if m.name not in self.elaborator_state.term_signatures:
                print(
                    f'ERROR: Unknown declared term name: {m.name.pretty()}')
                exit()
            ret_type = self.elaborator_state.term_signatures[m.name]
        elif isinstance(m, TypeAnnotationTerm):
            k = self.synth_type(ctx, m.type)
            if not isinstance(k, TypeKind):
                print(
                    f'ERROR: Mismatched kinds for annotation type: {m.type.pretty()}')
                print(
                    f'ERROR: Expected kind: {TypeKind(source=None).pretty()}')
                print(f'ERROR: Found kind: {k.pretty()}')
                exit()
            norm_ty = eval_type(ctx, m.type)
            self.check_term(ctx, norm_ty, m.term)
            ret_type = norm_ty
        elif isinstance(m, LambdaTerm):
            print(
                f'ERROR: Cannot synthesize type for lambda term {m.pretty()}')
            exit()
        elif isinstance(m, ApplicationTerm):
            fun_type = self.synth_term(ctx, m.function)
            if not isinstance(fun_type, FunctionType):
                print(
                    f'ERROR: Mismatched types for the function of a function application: {m.pretty()}')
                print(
                    f'ERROR: Expected a function type, but found: {fun_type.pretty()}')
                exit()
            self.check_term(ctx, fun_type.argument_type, m.argument)
            ret_type = fun_type.return_type
        elif isinstance(m, AbstractionTerm):
            print(
                f'ERROR: Cannot synthesize type for abstraction term {m.pretty()}')
            exit()
        elif isinstance(m, InstantiationTerm):
            forall_type = self.synth_term(ctx, m.function)
            if not isinstance(forall_type, ForallType):
                print(
                    f'ERROR: Mismatched types for the function of an instantiation: {m.pretty()}')
                print(
                    f'ERROR: Expected a forall type, but found: {forall_type.pretty()}')
                exit()
            k = self.synth_type(ctx, m.argument)
            if k != forall_type.tyvar_kind:
                print(
                    f'ERROR: Mismatched kinds for the argument of an instantiation: {m.pretty()}')
                print(
                    f'ERROR: Expected kind: {forall_type.tyvar_kind.pretty()}')
                print(f'ERROR: Found kind: {k.pretty()}')
                exit()

            ret_type = eval_type(ctx.extend({forall_type.scope.names[0]: HasTypeValue(
                eval_type(ctx, m.argument))}), forall_type.scope.body)
        elif isinstance(m, ConstructorTerm):
            print(
                f'ERROR: Cannot synthesize type for constructor term {m.pretty()}')
            exit()
        elif isinstance(m, CaseTerm):
            scrutinee_types = [self.synth_term(ctx, n) for n in m.scrutinees]
            if len(m.clauses) == 0:
                print(f'ERROR: Cannot synthesize types for empty case term')
                exit()
            clause_types = [self.synth_clause(
                ctx, scrutinee_types, cls) for cls in m.clauses]
            uniqs = []
            for clsty in clause_types:
                if clsty not in uniqs:
                    uniqs.append(clsty)
            if len(uniqs) != 1:
                print(
                    f'ERROR: Found different clause types for difference case clausses')
                for clsty in uniqs:
                    print(f'ERROR: Found type: {clsty.pretty()}')
                exit()
            ret_type = uniqs[0]
        else:
            todo()
            raise

        self.elaborator_state.task_stack.pop()

        return ret_type

    def synth_clause(self, ctx: Context, pat_ts: List[Type], cls: CaseClause) -> Type:

        self.elaborator_state.task_stack.append(SynthClause(ctx, pat_ts, cls))

        # ensure non-overlapping names
        captured = [v for pat in cls.patterns for v in pat.captured_variables()]
        old = []
        repeated = []

        for v in captured:
            if v in old:
                if v not in repeated:
                    repeated.append(v)
            else:
                old.append(v)
        if len(repeated) != 0:
            print(
                f'ERROR: Found repeated variables in patterns {cls.patterns}')
            print(f'ERROR: Repeated: {" ".join(repeated)}')
            exit()

        new_bindings = {v: cast(ContextJudgment, j)
                        for pat_ty, pat in list(zip(pat_ts, cls.patterns))
                        for v, j in self.check_pattern(ctx, pat_ty, pat).items()}

        self.elaborator_state.task_stack.pop()

        return self.synth_term(ctx.extend(new_bindings), cls.body.body)

    def check_pattern(self, ctx: Context, t: Type, pat: Pattern) -> Dict[str, ContextJudgment]:

        self.elaborator_state.task_stack.append(CheckPattern(ctx, t, pat))

        ret_bindings: Dict[str, ContextJudgment]

        if isinstance(pat, CapturedVariablePattern):
            ret_bindings = {pat.var: HasType(t)}
        elif isinstance(pat, WildcardVariablePattern):
            ret_bindings = {}
        elif isinstance(t, ConstructorType) and isinstance(pat, ConstructorPattern):

            if pat.constructor not in self.elaborator_state.type_constructor_constructors[t.name]:
                print(
                    f'ERROR: The type constructor {t.name.pretty()} does not have a constructor named {pat.constructor}')
                exit()

            new_tycon_params, consig = self.elaborator_state.type_constructor_constructors[t.name][pat.constructor].open(
                ctx.names())

            ctx = ctx.extend({
                v: HasTypeValue(t)
                for v, t in zip(new_tycon_params, t.arguments)
            })

            new_type_params, termsig = consig.term_signature.open(ctx.names())

            if len(pat.type_arguments) != len(consig.type_parameters_kinds):
                print(
                    f'ERROR: Incorrect number of type arguments in constructor pattern {pat.pretty()}')
                print(f'Expected {len(consig.type_parameters_kinds)}')
                print(f'Found {len(pat.type_arguments)}')
                exit()

            ctx = ctx.extend({
                v: HasKind(k)
                for v, k in zip(new_type_params, consig.type_parameters_kinds)
            })

            if len(pat.arguments) != len(termsig.parameters):
                print(
                    f'ERROR: Incorrect number of arguments in constructor pattern {pat.pretty()}')
                print(f'Expected {len(termsig.parameters)}')
                print(f'Found {len(pat.arguments)}')
                exit()

            aggregate_bindings = {tvp.tyvar: cast(ContextJudgment, HasKind(k))
                                  for tvp, k in zip(pat.type_arguments, consig.type_parameters_kinds)
                                  if isinstance(tvp, CapturedTypeVariablePattern)}

            for (_, argty), arg in zip(termsig.parameters, pat.arguments):
                aggregate_bindings |= self.check_pattern(
                    ctx, eval_type(ctx, argty), arg)

            found_ty = eval_type(ctx, termsig.return_type)
            if found_ty != t:
                print(
                    f'ERROR: Constructor pattern does not inhabit given type: {pat.pretty()}')
                print(f'ERROR: Expected type: {t.pretty()}')
                print(f'ERROR: Actual type: {found_ty.pretty()}')
                exit()

            ret_bindings = aggregate_bindings

        else:
            todo()
            raise

        self.elaborator_state.task_stack.pop()

        return ret_bindings


def eval_type(ctx: Context, t: Type):
    if isinstance(t, VariableType):
        if t.name not in ctx.variables or not isinstance(ctx.variables[t.name], HasTypeValue):
            return t
        return cast(HasTypeValue, ctx.variables[t.name]).type_value
    elif isinstance(t, ConstructorType):
        return ConstructorType(
            source=t.source,
            name=t.name,
            arguments=[eval_type(ctx, arg) for arg in t.arguments])
    elif isinstance(t, FunctionType):
        return FunctionType(
            source=t.source,
            argument_type=eval_type(ctx, t.argument_type),
            return_type=eval_type(ctx, t.return_type))
    elif isinstance(t, ForallType):
        ([new_var], new_scope) = t.scope.open(ctx.names())
        return ForallType(
            source=t.source,
            tyvar_kind=t.tyvar_kind,
            scope=Scope(
                names=[new_var],
                body=eval_type(ctx.extend({new_var: HasKind(t.tyvar_kind)}), new_scope)))
    elif isinstance(t, LambdaType):
        ([new_var], new_scope) = t.body.open(ctx.names())
        return LambdaType(
            source=t.source,
            tyvar_kind=t.tyvar_kind,
            body=Scope(
                names=[new_var],
                body=eval_type(ctx.extend({new_var: HasKind(t.tyvar_kind)}), new_scope)))
    elif isinstance(t, ApplicationType):
        fun = eval_type(ctx, t.function)
        arg = eval_type(ctx, t.argument)
        if isinstance(fun, LambdaType):
            return eval_type(Context({fun.body.names[0]: HasTypeValue(arg)}), fun.body.body)
        else:
            return ApplicationType(
                source=t.source,
                function=fun,
                argument=arg)
    else:
        flag(t)
        todo()
