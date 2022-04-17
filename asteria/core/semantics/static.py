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
                raise errors.RepeatedTypeConstructorParameterError(
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
                raise errors.RepeatedTermConstructorTypeParameterError(
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
                raise errors.RepeatedTermParameterInTermConstructorDeclError(
                    tasks=self.elaborator_state.task_stack,
                    term_parameter_name=param,
                    declaration=condecl)
            previous_names.append(param)
            k = self.synth_type(ctx, type)
            if not isinstance(k, TypeKind):
                self.log(
                    f'ERROR: Found a constructor parameter with an incorrectly kinded type: {param} in signature for constructor {condecl.name}')
                self.log(
                    f'ERROR: Expected kind: {TypeKind(source=None).pretty()}')
                self.log(f'ERROR: Found kind: {k.pretty()}')
                exit()

        ctx = ctx.extend({v: HasType(t) for v, t in termsig.parameters})

        k = self.synth_type(ctx, termsig.return_type)
        if not isinstance(k, TypeKind):
            self.log(
                f'ERROR: Found a constructor declaration that returns an incorrectly kinded type: {termsig.return_type.pretty()} in signature for constructor {condecl.name}')
            self.log(f'ERROR: Expected kind: {TypeKind(source=None).pretty()}')
            self.log(f'ERROR: Found kind: {k.pretty()}')
            exit()

        if not isinstance(termsig.return_type, ConstructorType) or termsig.return_type.name != tycon:
            self.log(
                f'ERROR: Found a constructor declaration that returns an invalid type: {termsig.return_type.pretty()} in signature for constructor {condecl.name}')
            self.log(
                f'ERROR: Expected a constructor type using the type constructor {tycon.pretty()}')
            exit()

        self.log(
            f'Term constructor {DeclaredConstructorName(tycon,condecl.name).pretty()} : {tycon_level_sig.pretty()}')
        self.elaborator_state.type_constructor_constructors[tycon][condecl.name] = condecl.signature

        self.elaborator_state.task_stack.pop()

        return True

    def elab_term_decl(self, mods: List[str], termdecl: TermDeclaration):

        self.elaborator_state.task_stack.append(ElabTermDeclaration(termdecl))

        if termdecl.name in self.elaborator_state.term_signatures:
            self.log(f'ERROR: Term name {termdecl.name} is already defined.')

        k = self.synth_type(Context({}), termdecl.type)
        if not isinstance(k, TypeKind):
            self.log(f'ERROR: ...')
            exit()

        name = DeclaredTermName(mods, termdecl.name)

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
                self.log(f'ERROR: Type variable not in scope: {t.pretty()}')
                exit()
            elif not isinstance(ctx.variables[t.name], HasKind):
                self.log(
                    f'ERROR: Cannot synthesize a kind for a non-type variable: {t.pretty()}')
                exit()

            ret_kind = cast(HasKind, ctx.variables[t.name]).kind
        elif isinstance(t, ConstructorType):
            if t.name not in self.elaborator_state.type_constructor_signatures:
                self.log(f'ERROR: Unknown type constructor: {t.name.pretty()}')
                exit()
            sig = [
                tvk.kind for tvk in self.elaborator_state.type_constructor_signatures[t.name].parameters]
            for i, arg in enumerate(t.arguments):
                k = self.synth_type(ctx, arg)
                if k != sig[i]:
                    self.log(
                        f'ERROR: Mismatched kinds for type parameter {arg.pretty()} of constructor type {k.pretty()}')
                    self.log(f'ERROR: Expecting kind: {sig[i].pretty()}')
                    self.log(f'ERROR: Found kind: {k.pretty()}')
                    exit()

            ret_kind = TypeKind(source=None)
        elif isinstance(t, FunctionType):
            k = self.synth_type(ctx, t.argument_type)
            if not isinstance(k, TypeKind):
                self.log(
                    f'ERROR: Found a function type with an incorrectly kinded argument type: {t.pretty()}')
                self.log(
                    f'ERROR: Expecting kind: {TypeKind(source=None).pretty()}')
                self.log(f'ERROR: Found kind: {k.pretty()}')
                exit()

            k = self.synth_type(ctx, t.return_type)
            if not isinstance(k, TypeKind):
                self.log(
                    f'ERROR: Found a function type with an incorrectly kinded return type: {t.pretty()}')
                self.log(
                    f'ERROR: Expecting kind: {TypeKind(source=None).pretty()}')
                self.log(f'ERROR: Found kind: {k.pretty()}')
                exit()

            ret_kind = TypeKind(source=None)
        elif isinstance(t, ForallType):
            ([new_var], new_body) = t.scope.open(ctx.names())
            ctx2 = ctx.extend({new_var: HasKind(t.tyvar_kind)})
            k = self.synth_type(ctx2, new_body)
            if not isinstance(k, TypeKind):
                self.log(
                    f'ERROR: Found a forall type with an incorrectly kinded scope: {t.pretty()}')
                self.log(
                    f'ERROR: Expecting kind: {TypeKind(source=None).pretty()}')
                self.log(f'ERROR: Found kind: {k.pretty()}')
                exit()

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
                self.log(
                    f'ERROR: Found an application type with an incorrectly kinded function: {t.pretty()}')
                self.log(
                    f'ERROR: Expected a function kind but found: {k.pretty()}')
                exit()

            k2 = self.synth_type(ctx, t.argument)
            if k.argument_kind != k2:
                self.log(
                    f'ERROR: Found an application type with an incorrectly kinded argument: {t.pretty()}')
                self.log(f'ERROR: Expected kind: {k.argument_kind.pretty()}')
                self.log(f'ERROR: Found kind: {k2.pretty()}')
                exit()

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
                self.log(
                    f'ERROR: The type constructor {t.name.pretty()} does not have a constructor named {m.constructor}')
                exit()

            new_tycon_param_names, consig = self.elaborator_state.type_constructor_constructors[t.name][m.constructor].open(
                ctx.names())

            ctx = ctx.extend({
                v: HasTypeValue(t)
                for v, t in zip(new_tycon_param_names, t.arguments)
            })

            if len(m.type_arguments) != len(consig.type_parameters_kinds):
                self.log(
                    f'ERROR: Incorrect number of type arguments in constructor term {m.pretty()}')
                self.log(f'Expected {len(consig.type_parameters_kinds)}')
                self.log(f'Found {len(m.type_arguments)}')
                exit()

            new_typaram_names, termsig = consig.term_signature.open(
                ctx.names())

            for tyargkind, tyarg in zip(consig.type_parameters_kinds, m.type_arguments):
                k = self.synth_type(ctx, tyarg)
                if tyargkind != k:
                    self.log(
                        f'ERROR: Incorrect kind for type argument {tyarg.pretty()} in constructor term {m.pretty()}')
                    self.log(f'ERROR: Expected kind: {tyargkind.pretty()}')
                    self.log(f'ERROR: Found kind: {k.pretty()}')
                    exit()

            ctx = ctx.extend({
                tyvar: HasTypeValue(tyval)
                for tyvar, tyval in zip(new_typaram_names, m.type_arguments)
            })

            if len(m.arguments) != len(termsig.parameters):
                self.log(
                    f'ERROR: Incorrect number of arguments in constructor term {m.pretty()}')
                self.log(
                    f'Expected {len(termsig.parameters)}')
                self.log(f'Found {len(m.arguments)}')
                exit()

            for (_, argty), arg in zip(termsig.parameters, m.arguments):
                self.check_term(ctx, eval_type(
                    ctx, argty), arg)

            found_ty = eval_type(ctx, termsig.return_type)
            if found_ty != t:
                self.log(
                    f'ERROR: Constructor term does not inhabit given type: {m.pretty()}')
                self.log(f'ERROR: Expected type: {t.pretty()}')
                self.log(f'ERROR: Actual type: {found_ty.pretty()}')
                exit()
        else:
            t2 = self.synth_term(ctx, m)
            if t2 != t:
                self.log(
                    f'ERROR: Term with synthesized type does not inhabit expected type')
                self.log(f'ERROR: Term: {m.pretty()}')
                self.log(f'ERROR: Expected type: {t.pretty()}')
                self.log(f'ERROR: Synthesized type: {t2.pretty()}')
                exit()

        self.elaborator_state.task_stack.pop()

    def synth_term(self, ctx: Context, m: Term) -> Type:

        self.elaborator_state.task_stack.append(SynthTerm(ctx, m))

        ret_type: Type

        if isinstance(m, VariableTerm):
            if m.name not in ctx.variables:
                self.log(f'ERROR: Found an unbound variable: {m.pretty()}')
                exit()
            if not isinstance(ctx.variables[m.name], HasType):
                self.log(
                    f'ERROR: Variable {m.pretty()} is used as a term variable but is declared as a type variable.')
                exit()
            ret_type = cast(HasType, ctx.variables[m.name]).type
        elif isinstance(m, DeclaredTermNameTerm):
            if m.name not in self.elaborator_state.term_signatures:
                self.log(
                    f'ERROR: Unknown declared term name: {m.name.pretty()}')
                exit()
            ret_type = self.elaborator_state.term_signatures[m.name]
        elif isinstance(m, TypeAnnotationTerm):
            k = self.synth_type(ctx, m.type)
            if not isinstance(k, TypeKind):
                self.log(
                    f'ERROR: Mismatched kinds for annotation type: {m.type.pretty()}')
                self.log(
                    f'ERROR: Expected kind: {TypeKind(source=None).pretty()}')
                self.log(f'ERROR: Found kind: {k.pretty()}')
                exit()
            norm_ty = eval_type(ctx, m.type)
            self.check_term(ctx, norm_ty, m.term)
            ret_type = norm_ty
        elif isinstance(m, LambdaTerm):
            self.log(
                f'ERROR: Cannot synthesize type for lambda term {m.pretty()}')
            exit()
        elif isinstance(m, ApplicationTerm):
            fun_type = self.synth_term(ctx, m.function)
            if not isinstance(fun_type, FunctionType):
                self.log(
                    f'ERROR: Mismatched types for the function of a function application: {m.pretty()}')
                self.log(
                    f'ERROR: Expected a function type, but found: {fun_type.pretty()}')
                exit()
            self.check_term(ctx, fun_type.argument_type, m.argument)
            ret_type = fun_type.return_type
        elif isinstance(m, AbstractionTerm):
            self.log(
                f'ERROR: Cannot synthesize type for abstraction term {m.pretty()}')
            exit()
        elif isinstance(m, InstantiationTerm):
            forall_type = self.synth_term(ctx, m.function)
            if not isinstance(forall_type, ForallType):
                self.log(
                    f'ERROR: Mismatched types for the function of an instantiation: {m.pretty()}')
                self.log(
                    f'ERROR: Expected a forall type, but found: {forall_type.pretty()}')
                exit()
            k = self.synth_type(ctx, m.argument)
            if k != forall_type.tyvar_kind:
                self.log(
                    f'ERROR: Mismatched kinds for the argument of an instantiation: {m.pretty()}')
                self.log(
                    f'ERROR: Expected kind: {forall_type.tyvar_kind.pretty()}')
                self.log(f'ERROR: Found kind: {k.pretty()}')
                exit()

            ret_type = eval_type(ctx.extend({forall_type.scope.names[0]: HasTypeValue(
                eval_type(ctx, m.argument))}), forall_type.scope.body)
        elif isinstance(m, ConstructorTerm):
            self.log(
                f'ERROR: Cannot synthesize type for constructor term {m.pretty()}')
            exit()
        elif isinstance(m, CaseTerm):
            scrutinee_types = [self.synth_term(ctx, n) for n in m.scrutinees]
            if len(m.clauses) == 0:
                self.log(f'ERROR: Cannot synthesize types for empty case term')
                exit()
            clause_types = [self.synth_clause(
                ctx, scrutinee_types, cls) for cls in m.clauses]
            uniqs = []
            for clsty in clause_types:
                if clsty not in uniqs:
                    uniqs.append(clsty)
            if len(uniqs) != 1:
                self.log(
                    f'ERROR: Found different clause types for difference case clausses')
                for clsty in uniqs:
                    self.log(f'ERROR: Found type: {clsty.pretty()}')
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
            self.log(
                f'ERROR: Found repeated variables in patterns {cls.patterns}')
            self.log(f'ERROR: Repeated: {" ".join(repeated)}')
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
                self.log(
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
                self.log(
                    f'ERROR: Incorrect number of type arguments in constructor pattern {pat.pretty()}')
                self.log(f'Expected {len(consig.type_parameters_kinds)}')
                self.log(f'Found {len(pat.type_arguments)}')
                exit()

            ctx = ctx.extend({
                v: HasKind(k)
                for v, k in zip(new_type_params, consig.type_parameters_kinds)
            })

            if len(pat.arguments) != len(termsig.parameters):
                self.log(
                    f'ERROR: Incorrect number of arguments in constructor pattern {pat.pretty()}')
                self.log(f'Expected {len(termsig.parameters)}')
                self.log(f'Found {len(pat.arguments)}')
                exit()

            aggregate_bindings = {tvp.tyvar: cast(ContextJudgment, HasKind(k))
                                  for tvp, k in zip(pat.type_arguments, consig.type_parameters_kinds)
                                  if isinstance(tvp, CapturedTypeVariablePattern)}

            for (_, argty), arg in zip(termsig.parameters, pat.arguments):
                aggregate_bindings |= self.check_pattern(
                    ctx, eval_type(ctx, argty), arg)

            found_ty = eval_type(ctx, termsig.return_type)
            if found_ty != t:
                self.log(
                    f'ERROR: Constructor pattern does not inhabit given type: {pat.pretty()}')
                self.log(f'ERROR: Expected type: {t.pretty()}')
                self.log(f'ERROR: Actual type: {found_ty.pretty()}')
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
