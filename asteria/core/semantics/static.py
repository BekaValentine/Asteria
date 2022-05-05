from dataclasses import dataclass
from typing import List, Dict, Optional, Tuple, cast

from asteria.utils import *
from asteria.core.syntax.abstract import *
import asteria.core.semantics.errors as errors
from asteria.core.semantics.contexts import *
from asteria.core.semantics.tasks import *


@dataclass(eq=False)
class Equation(Syntax):
    index_variable_name: str
    assigned_type: Type

    def concrete(self, n, t):
        return f'{n} :=? {t}'

    def rename(self, renaming):
        if renaming == {}:
            return self
        if self.index_variable_name in renaming:
            return Equation(self.source, renaming[self.index_variable_name], self.assigned_type.rename(renaming))
        else:
            return Equation(self.source, self.index_variable_name, self.assigned_type.rename(renaming))


@dataclass(eq=False)
class ElaboratedConstructorTermSignature(Syntax):
    parameters: List[Tuple[str, Type]]
    return_type: Type
    equations: List[Equation]

    def concrete(self, ps, ret, eqs):
        if len(self.equations) == 0:
            eqsp = 'TRUE'
        else:
            eqsp = ' & '.join(eqs)

        if len(ps) == 0:
            return f'{ret} & {eqsp}'
        else:
            psp = ' '.join([
                f'({n} : {t})'
                for n, t in ps
            ])
            return f'{psp} ~> {ret} & {eqsp}'


@dataclass(eq=False)
class ElaboratedConstructorSignature(Syntax):
    type_parameters_kinds: List[Kind]
    term_signature: Scope[ElaboratedConstructorTermSignature]

    def concrete(self, ks, tsigsc):
        ns, tsig = tsigsc

        if len(ks) == 0:
            return tsig
        else:
            typarams = ' '.join([
                f'{{{n} : {k}}}'
                for n, k in zip(ns, ks)
            ])
            return f'{typarams} ~> {tsig}'


def ElaboratedConstructorSignature_from_ConstructorSignature(sig: Scope[ConstructorSignature]) -> Scope[ElaboratedConstructorSignature]:
    new_names = fresh_variables(sig.body.term_signature.names, sig.names)
    new_args: List[Type] = [VariableType(None, n) for n in new_names]
    new_ret_type = ConstructorType(
        None, cast(ConstructorType, sig.body.term_signature.body.return_type).name, new_args)
    eqs: List[Equation] = [Equation(source=None, index_variable_name=l, assigned_type=r) for l, r in zip(new_names, cast(
        ConstructorType, sig.body.term_signature.body.return_type).arguments)]

    return Scope(
        names=new_names,
        body=ElaboratedConstructorSignature(
            source=sig.body.source,
            type_parameters_kinds=sig.body.type_parameters_kinds,
            term_signature=Scope(
                names=sig.body.term_signature.names,
                body=ElaboratedConstructorTermSignature(
                    source=sig.body.term_signature.body.source,
                    parameters=sig.body.term_signature.body.parameters,
                    return_type=new_ret_type,
                    equations=eqs))))


@dataclass
class ElaboratorState(object):
    type_constructor_signatures: Dict[DeclaredTypeConstructorName,
                                      TypeConstructorSignature]
    type_constructor_constructors: Dict[DeclaredTypeConstructorName,
                                        Dict[str, Tuple[Scope[ConstructorSignature], Scope[ElaboratedConstructorSignature]]]]
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

        params = [tvk.tyvar for tvk in decl.signature.arguments]
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

        new_tycon_arg_names, tycon_level_sig = condecl.signature.open([
            p.tyvar
            for p in tycon_sig.arguments
        ])

        param_markers: List[Optional[Tuple[str, str]]] = []
        i = 0
        for arg in tycon_sig.arguments:
            if isinstance(arg, ParameterArgument):
                param_markers.append((arg.tyvar, new_tycon_arg_names[i]))
                i += 1
            else:
                param_markers.append(None)

        ctx = Context({typaram: HasKind(arg.kind)
                       for typaram, arg in zip(new_tycon_arg_names, tycon_sig.arguments)})

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

        for mark, (i, ret_ty_arg) in zip(param_markers, enumerate(termsig.return_type.arguments)):
            if mark is not None:
                if not isinstance(ret_ty_arg, VariableType) or ret_ty_arg.name != mark[1]:
                    raise errors.TypeParameterNotUniformInConstructors(
                        tasks=self.elaborator_state.task_stack,
                        declaration=condecl,
                        argument=cast(
                            ConstructorType, condecl.signature.body.term_signature.body.return_type).arguments[i],
                        expected_parameter=mark[0])

        new_consig = ElaboratedConstructorSignature_from_ConstructorSignature(
            condecl.signature)
        argsp = 'args ' + ', '.join(new_consig.names) + '. '
        self.log(
            f'Term constructor {DeclaredConstructorName(tycon,condecl.name).pretty()} : {argsp}{new_consig.body.pretty()}')
        self.elaborator_state.type_constructor_constructors[tycon][condecl.name] = (
            condecl.signature, new_consig)

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
                tvk.kind for tvk in self.elaborator_state.type_constructor_signatures[t.name].arguments]

            if len(sig) != len(t.arguments):
                todo()
                raise

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

        if isinstance(m, LambdaTerm):
            if not isinstance(t, FunctionType):
                raise errors.CannotCheckLambdaTermAgainstNonFunctionTypeError(
                    tasks=self.elaborator_state.task_stack,
                    term=m,
                    type=t)

            ([new_var], new_body) = m.body.open(ctx.names())
            self.check_term(ctx.extend(
                {new_var: HasType(t.argument_type)}), t.return_type, new_body)

        elif isinstance(m, AbstractionTerm):
            if not isinstance(t, ForallType):
                raise errors.CannotCheckAbstractionTermAgainstNonForallTypeError(
                    tasks=self.elaborator_state.task_stack,
                    term=m,
                    type=t)

            ([new_var], new_type_scope) = t.scope.open(ctx.names())
            new_body = m.body.body.rename({m.body.names[0]: new_var})
            self.check_term(ctx.extend(
                {new_var: HasKind(t.tyvar_kind)}), new_type_scope, new_body)

        elif isinstance(m, ConstructorTerm):
            if not isinstance(t, ConstructorType):
                raise errors.CannotCheckConstructorTermAgainstNonConstructorTypeError(
                    tasks=self.elaborator_state.task_stack,
                    term=m,
                    type=t)

            if m.constructor not in self.elaborator_state.type_constructor_constructors[t.name]:
                known: Dict[str, Scope[ConstructorSignature]] = {
                    k: v
                    for k, (v, _) in self.elaborator_state.type_constructor_constructors[t.name].items()
                }
                raise errors.UnknownTermConstructorForTypeError(
                    tasks=self.elaborator_state.task_stack,
                    term=m,
                    type=t,
                    known_term_constructors=known)

            new_tycon_arg_names, consig = self.elaborator_state.type_constructor_constructors[t.name][m.constructor][1].open(
                ctx.names())

            if len(m.type_arguments) != len(consig.type_parameters_kinds):
                raise errors.IncorrectNumberOfTypeArgumentsInConstructorTermError(
                    tasks=self.elaborator_state.task_stack,
                    term=m,
                    expected_number=len(consig.type_parameters_kinds),
                    found_number=len(m.type_arguments))

            new_typaram_names, termsig = consig.term_signature.open(
                ctx.names())

            for tyargkind, tyarg in zip(consig.type_parameters_kinds, m.type_arguments):
                k = self.synth_type(ctx, tyarg)
                if tyargkind != k:
                    raise errors.IncorrectKindForTypeArgumentInConstructorTermError(
                        tasks=self.elaborator_state.task_stack,
                        type_argument=tyarg,
                        term=m,
                        expected_kind=tyargkind,
                        found_kind=k)

            ctx = ctx.extend({
                tyvar: HasTypeValue(tyval)
                for tyvar, tyval in zip(new_typaram_names, m.type_arguments)
            })

            ctx = ctx.extend({
                n: HasTypeValue(t)
                for n, t in zip(new_tycon_arg_names, t.arguments)
            })

            for eq in termsig.equations:
                el = eval_type(ctx, VariableType(None, eq.index_variable_name))
                er = eval_type(ctx, eq.assigned_type)
                if el != er:

                    found_ty = eval_type(ctx,
                                         ConstructorType(
                                             None,
                                             cast(
                                                 ConstructorType, termsig.return_type).name,
                                             [eq.assigned_type for eq in termsig.equations]))

                    raise errors.ConstructorTermDoesNotInhabitCheckTypeError(
                        tasks=self.elaborator_state.task_stack,
                        term=m,
                        expected_type=eval_type(ctx, t),
                        found_type=found_ty)

            if len(m.arguments) != len(termsig.parameters):
                raise errors.IncorrectNumberOfArgumentsInConstructorTermError(
                    tasks=self.elaborator_state.task_stack,
                    term=m,
                    expected_number=len(termsig.parameters),
                    found_number=len(m.arguments))

            for (_, argty), arg in zip(termsig.parameters, m.arguments):
                self.check_term(ctx, eval_type(
                    ctx, argty), arg)
        else:
            t2 = self.synth_term(ctx, m)
            if t2 != t:
                raise errors.IncorrectTypeForSynthesizedTermError(
                    tasks=self.elaborator_state.task_stack,
                    term=m,
                    expected_type=t,
                    found_type=t2)

        self.elaborator_state.task_stack.pop()

    def synth_term(self, ctx: Context, m: Term) -> Type:

        self.elaborator_state.task_stack.append(SynthTerm(ctx, m))

        ret_type: Type

        if isinstance(m, VariableTerm):
            if m.name not in ctx.variables:
                raise errors.VariableNotInScopeError(
                    tasks=self.elaborator_state.task_stack,
                    term=m,
                    context=ctx)
            if not isinstance(ctx.variables[m.name], HasType):
                raise errors.TypeVariableUsedAsTermVariableError(
                    tasks=self.elaborator_state.task_stack,
                    term=m,
                    context=ctx)
            ret_type = cast(HasType, ctx.variables[m.name]).type
        elif isinstance(m, DeclaredTermNameTerm):
            if m.name not in self.elaborator_state.term_signatures:
                raise errors.DeclaredTermNameNotInScopeError(
                    tasks=self.elaborator_state.task_stack,
                    term=m,
                    known_term_names=self.elaborator_state.term_signatures)
            ret_type = self.elaborator_state.term_signatures[m.name]
        elif isinstance(m, TypeAnnotationTerm):
            k = self.synth_type(ctx, m.type)
            if not isinstance(k, TypeKind):
                raise errors.IncorrectKindForAnnotationTypeError(
                    tasks=self.elaborator_state.task_stack,
                    term=m,
                    found_kind=k)
            norm_ty = eval_type(ctx, m.type)
            self.check_term(ctx, norm_ty, m.term)
            ret_type = norm_ty
        elif isinstance(m, LambdaTerm):
            raise errors.CannotSynthesizeTypeForLambdaTermError(
                tasks=self.elaborator_state.task_stack,
                term=m)
        elif isinstance(m, ApplicationTerm):
            fun_type = self.synth_term(ctx, m.function)
            if not isinstance(fun_type, FunctionType):
                raise errors.IncorrectTypeForApplicationTermFunctionError(
                    tasks=self.elaborator_state.task_stack,
                    term=m,
                    found_type=fun_type)
            self.check_term(ctx, fun_type.argument_type, m.argument)
            ret_type = fun_type.return_type
        elif isinstance(m, AbstractionTerm):
            raise errors.CannotSynthesizeTypeForAbstractionTermError(
                tasks=self.elaborator_state.task_stack,
                term=m)
        elif isinstance(m, InstantiationTerm):
            forall_type = self.synth_term(ctx, m.function)
            if not isinstance(forall_type, ForallType):
                raise errors.IncorrectTypeForInstantiationTermFunctionError(
                    tasks=self.elaborator_state.task_stack,
                    term=m,
                    found_type=forall_type)
            k = self.synth_type(ctx, m.argument)
            if k != forall_type.tyvar_kind:
                raise errors.IncorrectKindForInstantiationTermArgumentError(
                    tasks=self.elaborator_state.task_stack,
                    term=m,
                    expected_kind=forall_type.tyvar_kind,
                    found_kind=k)

            ret_type = eval_type(ctx.extend({forall_type.scope.names[0]: HasTypeValue(
                eval_type(ctx, m.argument))}), forall_type.scope.body)
        elif isinstance(m, ConstructorTerm):
            raise errors.CannotSynthesizeTypeForConstructorTermError(
                tasks=self.elaborator_state.task_stack,
                term=m)
        elif isinstance(m, CaseTerm):
            scrutinee_types = [self.synth_term(ctx, n) for n in m.scrutinees]
            if len(m.clauses) == 0:
                raise errors.CannotSynthesizeTypeForEmptyCaseTermError(
                    tasks=self.elaborator_state.task_stack,
                    term=m)
            clause_types = [
                (cls, self.synth_clause(ctx, scrutinee_types, cls))
                for cls in m.clauses]

            clause_type = None
            mismatched = False

            for _, clsty in clause_types:
                if clause_type is None:
                    clause_type = clsty
                elif clause_type != clsty:
                    mismatched = True
            if mismatched:
                raise errors.DifferentTypesForCaseClausesError(
                    tasks=self.elaborator_state.task_stack,
                    term=m,
                    clauses=clause_types)
            if clause_type is None:
                raise
            ret_type = clause_type
        else:
            todo()
            raise

        self.elaborator_state.task_stack.pop()

        return ret_type

    def synth_clause(self, ctx: Context, pat_ts: List[Type], cls: CaseClause) -> Type:

        self.elaborator_state.task_stack.append(SynthClause(ctx, pat_ts, cls))

        if len(pat_ts) != len(cls.patterns):
            raise errors.IncorrectNumberOfPatternsInCaseClauseError(
                    tasks=self.elaborator_state.task_stack,
                    clause=cls,
                    expected_number=len(pat_ts),
                    found_number=len(cls.patterns))

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
            raise errors.RepeatedVariablesInCaseClausePatternsError(
                tasks=self.elaborator_state.task_stack,
                clause=cls,
                repeated_variables=repeated)

        new_binding_types = [
            j
            for pat_ty, pat in list(zip(pat_ts, cls.patterns))
            for j in self.check_pattern(ctx, pat_ty, pat)
        ]

        new_bindings = {
            v: cast(ContextJudgment, j)
            for v, j in zip(cls.body.names, new_binding_types)
        }

        self.elaborator_state.task_stack.pop()

        return self.synth_term(ctx.extend(new_bindings), cls.body.body)

    def check_pattern(self, ctx: Context, t: Type, pat: Pattern) -> List[ContextJudgment]:

        self.elaborator_state.task_stack.append(CheckPattern(ctx, t, pat))

        ret_bindings: List[ContextJudgment]

        if isinstance(pat, CapturedVariablePattern):
            ret_bindings = [HasType(t)]
        elif isinstance(pat, WildcardVariablePattern):
            ret_bindings = []
        elif isinstance(t, ConstructorType) and isinstance(pat, ConstructorPattern):

            if pat.constructor not in self.elaborator_state.type_constructor_constructors[t.name]:
                known: Dict[str, Scope[ConstructorSignature]] = {
                    k: v
                    for k, (v, _) in self.elaborator_state.type_constructor_constructors[t.name].items()
                }
                raise errors.UnknownTermConstructorForTypePatternError(
                    tasks=self.elaborator_state.task_stack,
                    pattern=pat,
                    type=t,
                    known_term_constructors=known)

            # a constructor has a signature like  (a : Type), [b : Type]. {c : Type} -> (x : c) -> Foo(a,b) & b :=? List(c)
            # so we open it to get a' and b' fresh, togther with the body {c : Type} -> ...
            new_tycon_arg_names, consig = self.elaborator_state.type_constructor_constructors[t.name][pat.constructor][1].open(
                ctx.names())

            # the incoming type for the patter is, say, Foo(A, List(C))
            # so one we open to get a' and b', we also extend the environment
            # with the knowledge a' := A, b' = List(C)
            # so effectively the consig is {c : Type} -> (x : c) -> Foo(A,List(C)) & List(C) :=? List(c)
            ctx = ctx.extend({
                v: HasTypeValue(t)
                for v, t in zip(new_tycon_arg_names, t.arguments)
            })

            if len(pat.type_arguments) != len(consig.type_parameters_kinds):
                raise errors.IncorrectNumberOfTypeArgumentsInConstructorPatternError(
                    tasks=self.elaborator_state.task_stack,
                    pattern=pat,
                    expected_number=len(consig.type_parameters_kinds),
                    found_number=len(pat.type_arguments))

            # check that the assertion types are valid
            for k, tp in zip(consig.type_parameters_kinds, pat.type_arguments):
                if isinstance(tp, AssertionTypePattern):

                    found_k = self.synth_type(ctx, tp.type)
                    if k != found_k:
                        raise errors.IncorrectKindForTypeArgumentInConstructorPatternError(
                            tasks=self.elaborator_state.task_stack,
                            type_argument=tp.type,
                            pattern=pat,
                            expected_kind=k,
                            found_kind=found_k)

            # we open the type params too:
            # so we get c' fresh, and the body (x : c') -> Foo(a',b') & b' :=? List(c')
            # which thanks to above assignments of type valus is basically
            # (x : c') -> Foo(A,List(C)) & List(C) =? List(c')
            new_type_params, termsig = consig.term_signature.open(ctx.names())

            # we add the actual asserted type arguments into the context
            ctx = ctx.extend({
                n: HasTypeValue(tp.type)
                for n, tp in zip(new_type_params, pat.type_arguments)
                if isinstance(tp, AssertionTypePattern)
            }).extend({
                n: HasKind(k)
                for n, k in zip(new_type_params, consig.type_parameters_kinds)
                if isinstance(tp, CapturedVariableTypePattern)
            })

            # named type arguments in the pattern, such as d in Bar(d; x) are captured, but the
            # name itself doesn't actually matter, so the only one that matters is the c/c' from above
            # lets build a mapping from consig names to whether or not they're captured
            capture_info = {
                v: isinstance(p, CapturedVariableTypePattern)
                for v, p in zip(new_type_params, pat.type_arguments)
            }

            for eq in termsig.equations:
                el = eval_type(ctx, VariableType(None, eq.index_variable_name))
                er = eval_type(ctx, eq.assigned_type)

                if el != er:
                    found_ty = eval_type(ctx.extend({v: HasTypeValue(VariableType(t.source, cast(CapturedVariableTypePattern, t).tyvar)) for v, t in zip(
                        new_type_params, pat.type_arguments) if isinstance(t, CapturedVariableTypePattern)}),
                                         ConstructorType(
                                             None,
                                             cast(ConstructorType,
                                                  termsig.return_type).name,
                                             [eq.assigned_type for eq in termsig.equations]))

                    raise errors.ConstructorPatternDoesNotInhabitCheckTypeError(
                        tasks=self.elaborator_state.task_stack,
                        pattern=pat,
                        expected_type=t,
                        found_type=found_ty)

            if len(pat.arguments) != len(termsig.parameters):
                raise errors.IncorrectNumberOfArgumentsInConstructorPatternError(
                    tasks=self.elaborator_state.task_stack,
                    pattern=pat,
                    expected_number=len(termsig.parameters),
                    found_number=len(pat.arguments))

            aggregate_bindings: List[ContextJudgment] = [
                HasKind(k)
                for tvp, k in zip(pat.type_arguments, consig.type_parameters_kinds)
                if isinstance(tvp, CapturedVariableTypePattern)
            ]

            for (_, argty), arg in zip(termsig.parameters, pat.arguments):
                aggregate_bindings += self.check_pattern(
                    ctx, eval_type(ctx, argty), arg)

            ret_bindings = aggregate_bindings

        else:
            flag(pat)
            flag(t)
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


# def match_type(ctx: Context, ty: Type, pat: TypePattern) -> bool:
#     if isinstance(pat, WildcardVariableTypePattern):
#         return True
#     elif isinstance(ty, VariableType) and isinstance(pat, CapturedVariableTypePattern):
#         return ty.name == pat.tyvar
#     elif isinstance(ty, ConstructorType) and isinstance(pat, ConstructorTypePattern):
#         return ty.name == pat.name and\
#             len(ty.arguments) == len(pat.arguments) and\
#             all(match_type(ctx, l, r)
#                 for l, r in zip(ty.arguments, pat.arguments))
#     elif isinstance(ty, FunctionType) and isinstance(pat, FunctionTypePattern):
#         return match_type(ctx, ty.argument_type, pat.argument_type) and\
#             match_type(ctx, ty.return_type, pat.return_type)
#     elif isinstance(ty, ForallType) and isinstance(pat, ForallTypePattern):
#         [new_tyvar], ty_scope = ty.scope.open(ctx.names())
#         pat_scope = pat.scope.body.rename({
#             pat.scope.names[0]: new_tyvar
#         })
#
#         return ty.tyvar_kind == pat.tyvar_kind and\
#             match_type(ctx.extend({
#                 new_tyvar: HasKind(ty.tyvar_kind)
#             }), ty_scope, pat_scope)
#     elif isinstance(ty, LambdaType) and isinstance(pat, LambdaTypePattern):
#         [new_tyvar], ty_body = ty.body.open(ctx.names())
#         pat_body = pat.body.body.rename({
#             pat.body.names[0]: new_tyvar
#         })
#
#         return ty.tyvar_kind == pat.tyvar_kind and\
#             match_type(ctx.extend({
#                 new_tyvar: HasKind(ty.tyvar_kind)
#             }), ty_body, pat_body)
#     elif isinstance(ty, ApplicationType) and isinstance(pat, ApplicationTypePattern):
#         return match_type(ctx, ty.function, pat.function) and\
#             match_type(ctx, ty.argument, pat.argument)
#     else:
#         return False
