from dataclasses import dataclass
from typing import List, Dict, Optional, Tuple, cast

from asteria.utils import *
from asteria.core.syntax.abstract import *


@dataclass
class ContextJudgment(object):
    pass


@dataclass
class HasType(ContextJudgment):
    type: Type


@dataclass
class HasKind(ContextJudgment):
    kind: Kind


@dataclass
class HasTypeValue(ContextJudgment):
    type_value: Type


@dataclass
class Context(object):
    variables: Dict[str, ContextJudgment]

    def names(self):
        return list(self.variables.keys())

    def lookup_variable(self, name: str) -> Optional[ContextJudgment]:
        return self.variables.get(name)

    def extend(self, names: Dict[str, ContextJudgment]):
        # args2 = {k: self.variables[k] for k in self.variables}
        # for k in names:
        #     args2[k] = names[k]
        # return Context(args2)
        return Context(self.variables | names)


class Elaborator(object):
    type_constructor_signatures: Dict[DeclaredTypeConstructorName,
                                      TypeConstructorSignature]
    type_constructor_constructors: Dict[DeclaredTypeConstructorName,
                                        Dict[str, Scope[ConstructorSignature]]]
    term_signatures: Dict[DeclaredTermName, Type]
    term_definitions: Dict[DeclaredTermName, Term]

    def __init__(self):
        self.type_constructor_signatures = {}
        self.type_constructor_constructors = {}
        self.term_signatures = {}
        self.term_definitions = {}

    def elab_module(self, name: List[str], mod: Module):
        for decl in mod.declarations:
            if isinstance(decl, DataDeclaration):
                self.elab_data_decl(name, decl)
            elif isinstance(decl, TermDeclaration):
                self.elab_term_decl(name, decl)

    def elab_data_decl(self, mods: List[str], decl: DataDeclaration):
        tycon = DeclaredTypeConstructorName(
            modules=mods,
            name=decl.name)

        if tycon in self.type_constructor_signatures:
            print(
                f'ERROR: Type constructor {tycon.pretty()} is already defined.')
            exit()

        print(
            f'Type constructor {tycon.pretty()} {decl.signature.pretty()}')

        self.type_constructor_signatures[tycon] = decl.signature
        self.type_constructor_constructors[tycon] = {}

        for condecl in decl.constructors:
            self.elab_condecl(tycon, decl.signature, condecl)

    def elab_condecl(self, tycon: DeclaredTypeConstructorName, tycon_sig: TypeConstructorSignature, condecl: ConstructorDeclaration) -> bool:
        if condecl.name in self.type_constructor_constructors[tycon]:
            print(
                f'ERROR: Constructor {condecl.name} is already defined for type constructor {tycon.pretty()}')
            exit()

        new_tycon_param_names, tycon_level_sig = condecl.signature.open([])

        ctx = Context({typaram: HasKind(tvk.kind)
                      for typaram, tvk in zip(new_tycon_param_names, tycon_sig.parameters)})

        previous_names = []

        for type_parameter in tycon_level_sig.term_signature.names:
            if type_parameter in previous_names:
                print(
                    f'ERROR: Repeated variable {type_parameter} in signature for constructor {condecl.name}')
                exit()
            previous_names.append(type_parameter)

        new_typaram_names, termsig = tycon_level_sig.term_signature.open(
            ctx.names())

        ctx = ctx.extend({
            v: HasKind(k)
            for v, k in zip(new_typaram_names, tycon_level_sig.type_parameters_kinds)
        })

        for param, type in termsig.parameters:
            if param in previous_names:
                print(
                    f'ERROR: Repeated variable {param} in signature for constructor {condecl.name}')
                exit()
            previous_names.append(param)
            k = self.synth_type(ctx, type)
            if k != TypeKind():
                print(
                    f'ERROR: Found a constructor parameter with an incorrectly kinded type: {param} in signature for constructor {condecl.name}')
                print(f'ERROR: Expected kind: {TypeKind().pretty()}')
                print(f'ERROR: Found kind: {k.pretty()}')
                exit()

        ctx = ctx.extend({v: HasType(t) for v, t in termsig.parameters})

        k = self.synth_type(ctx, termsig.return_type)
        if k != TypeKind():
            print(
                f'ERROR: Found a constructor declaration that returns an incorrectly kinded type: {termsig.return_type.pretty()} in signature for constructor {condecl.name}')
            print(f'ERROR: Expected kind: {TypeKind().pretty()}')
            print(f'ERROR: Found kind: {k.pretty()}')
            exit()

        if not isinstance(termsig.return_type, ConstructorType) or termsig.return_type.name != tycon:
            print(
                f'ERROR: Found a constructor declaration that returns an invalid type: {termsig.return_type.pretty()} in signature for constructor {condecl.name}')
            print(
                f'ERROR: Expected a constructor type using the type constructor {tycon.pretty()}')
            exit()

        print(
            f'Term constructor {DeclaredConstructorName(tycon,condecl.name).pretty()} : {tycon_level_sig.pretty()}')
        self.type_constructor_constructors[tycon][condecl.name] = condecl.signature

        return True

    def elab_term_decl(self, mods: List[str], termdecl: TermDeclaration):

        if termdecl.name in self.term_signatures:
            print(f'ERROR: Term name {termdecl.name} is already defined.')

        k = self.synth_type(Context({}), termdecl.type)
        if k != TypeKind():
            print(f'ERROR: ...')
            exit()

        name = DeclaredTermName(mods, termdecl.name)

        print(
            f'Term name {name.pretty()} : {termdecl.type.pretty()}')
        self.term_signatures[name] = termdecl.type

        self.check_term(Context({}), termdecl.type, termdecl.definition)

        self.term_definitions[name] = termdecl.definition

    def synth_type(self, ctx: Context, t: Type) -> Kind:
        if isinstance(t, VariableType):
            if t.name not in ctx.variables:
                print(f'ERROR: Type variable not in scope: {t.pretty()}')
                exit()
            elif not isinstance(ctx.variables[t.name], HasKind):
                print(
                    f'ERROR: Cannot synthesize a kind for a non-type variable: {t.pretty()}')
                exit()

            return cast(HasKind, ctx.variables[t.name]).kind
        elif isinstance(t, ConstructorType):
            if t.name not in self.type_constructor_signatures:
                print(f'ERROR: Unknown type constructor: {t.name.pretty()}')
                exit()
            sig = [
                tvk.kind for tvk in self.type_constructor_signatures[t.name].parameters]
            for i, arg in enumerate(t.arguments):
                k = self.synth_type(ctx, arg)
                if k != sig[i]:
                    print(
                        f'ERROR: Mismatched kinds for type parameter {arg.pretty()} of constructor type {k.pretty()}')
                    print(f'ERROR: Expecting kind: {sig[i].pretty()}')
                    print(f'ERROR: Found kind: {k.pretty()}')
                    exit()

            return TypeKind()
        elif isinstance(t, FunctionType):
            k = self.synth_type(ctx, t.argument_type)
            if k != TypeKind():
                print(
                    f'ERROR: Found a function type with an incorrectly kinded argument type: {t.pretty()}')
                print(f'ERROR: Expecting kind: {TypeKind().pretty()}')
                print(f'ERROR: Found kind: {k.pretty()}')
                exit()

            k = self.synth_type(ctx, t.return_type)
            if k != TypeKind():
                print(
                    f'ERROR: Found a function type with an incorrectly kinded return type: {t.pretty()}')
                print(f'ERROR: Expecting kind: {TypeKind().pretty()}')
                print(f'ERROR: Found kind: {k.pretty()}')
                exit()

            return TypeKind()
        elif isinstance(t, ForallType):
            ([new_var], new_body) = t.scope.open(ctx.names())
            ctx2 = ctx.extend({new_var: HasKind(t.tyvar_kind)})
            k = self.synth_type(ctx2, new_body)
            if k != TypeKind():
                print(
                    f'ERROR: Found a forall type with an incorrectly kinded scope: {t.pretty()}')
                print(f'ERROR: Expecting kind: {TypeKind().pretty()}')
                print(f'ERROR: Found kind: {k.pretty()}')
                exit()

            return TypeKind()
        elif isinstance(t, LambdaType):
            ([new_var], new_body) = t.body.open(ctx.names())
            ctx2 = ctx.extend({new_var: HasKind(t.tyvar_kind)})
            k = self.synth_type(ctx2, new_body)

            return FunctionKind(argument_kind=t.tyvar_kind, return_kind=k)
        elif isinstance(t, ApplicationType):
            k = self.synth_type(ctx, t.function)

            if not isinstance(k, FunctionKind):
                print(
                    f'ERROR: Found an application type with an incorrectly kinded function: {t.pretty()}')
                print(
                    f'ERROR: Expected a function kind but found: {k.pretty()}')
                exit()

            k2 = self.synth_type(ctx, t.argument)
            if k.argument_kind != k2:
                print(
                    f'ERROR: Found an application type with an incorrectly kinded argument: {t.pretty()}')
                print(f'ERROR: Expected kind: {k.argument_kind.pretty()}')
                print(f'ERROR: Found kind: {k2.pretty()}')
                exit()

            return k.return_kind
        else:
            todo()
            raise

    def check_term(self, ctx: Context, t: Type, m: Term) -> None:
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
            if m.constructor not in self.type_constructor_constructors[t.name]:
                print(
                    f'ERROR: The type constructor {t.name.pretty()} does not have a constructor named {m.constructor}')
                exit()

            new_tycon_param_names, consig = self.type_constructor_constructors[t.name][m.constructor].open(
                ctx.names())

            ctx = ctx.extend({
                v: HasTypeValue(t)
                for v, t in zip(new_tycon_param_names, t.arguments)
            })

            if len(m.type_arguments) != len(consig.type_parameters_kinds):
                print(
                    f'ERROR: Incorrect number of type arguments in constructor term {m.pretty()}')
                print(f'Expected {len(consig.type_parameters_kinds)}')
                print(f'Found {len(m.type_arguments)}')
                exit()

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

    def synth_term(self, ctx: Context, m: Term) -> Type:
        if isinstance(m, VariableTerm):
            if m.name not in ctx.variables:
                print(f'ERROR: Found an unbound variable: {m.pretty()}')
                exit()
            if not isinstance(ctx.variables[m.name], HasType):
                print(
                    f'ERROR: Variable {m.pretty()} is used as a term variable but is declared as a type variable.')
                exit()
            return cast(HasType, ctx.variables[m.name]).type
        elif isinstance(m, DeclaredTermNameTerm):
            if m.name not in self.term_signatures:
                print(f'ERROR: Unknown declared term name: {m.name.pretty()}')
                exit()
            return self.term_signatures[m.name]
        elif isinstance(m, TypeAnnotationTerm):
            k = self.synth_type(ctx, m.type)
            if k != TypeKind():
                print(
                    f'ERROR: Mismatched kinds for annotation type: {m.type.pretty()}')
                print(f'ERROR: Expected kind: {TypeKind().pretty()}')
                print(f'ERROR: Found kind: {k.pretty()}')
                exit()
            norm_ty = eval_type(ctx, m.type)
            self.check_term(ctx, norm_ty, m.term)
            return norm_ty
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
            return fun_type.return_type
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

            return eval_type(ctx.extend({forall_type.scope.names[0]: HasTypeValue(eval_type(ctx, m.argument))}), forall_type.scope.body)
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
            return uniqs[0]
        else:
            todo()
            raise

    def synth_clause(self, ctx: Context, pat_ts: List[Type], cls: CaseClause) -> Type:
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

        return self.synth_term(ctx.extend(new_bindings), cls.body.body)

    def check_pattern(self, ctx: Context, t: Type, pat: Pattern) -> Dict[str, ContextJudgment]:

        if isinstance(pat, CapturedVariablePattern):
            return {pat.var: HasType(t)}
        elif isinstance(pat, WildcardVariablePattern):
            return {}
        elif isinstance(t, ConstructorType) and isinstance(pat, ConstructorPattern):

            if pat.constructor not in self.type_constructor_constructors[t.name]:
                print(
                    f'ERROR: The type constructor {t.name.pretty()} does not have a constructor named {pat.constructor}')
                exit()

            new_tycon_params, consig = self.type_constructor_constructors[t.name][pat.constructor].open(
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

            return aggregate_bindings

        else:
            todo()
            raise


def eval_type(ctx: Context, t: Type):
    if isinstance(t, VariableType):
        if t.name not in ctx.variables or not isinstance(ctx.variables[t.name], HasTypeValue):
            return t
        return cast(HasTypeValue, ctx.variables[t.name]).type_value
    elif isinstance(t, ConstructorType):
        return ConstructorType(
            name=t.name,
            arguments=[eval_type(ctx, arg) for arg in t.arguments])
    elif isinstance(t, FunctionType):
        return FunctionType(
            argument_type=eval_type(ctx, t.argument_type),
            return_type=eval_type(ctx, t.return_type))
    elif isinstance(t, ForallType):
        ([new_var], new_scope) = t.scope.open(ctx.names())
        return ForallType(
            tyvar_kind=t.tyvar_kind,
            scope=Scope(
                names=[new_var],
                body=eval_type(ctx.extend({new_var: HasKind(t.tyvar_kind)}), new_scope)))
    elif isinstance(t, LambdaType):
        ([new_var], new_scope) = t.body.open(ctx.names())
        return LambdaType(
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
                function=fun,
                argument=arg)
    else:
        flag(t)
        todo()
