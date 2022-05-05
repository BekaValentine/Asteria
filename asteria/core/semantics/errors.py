import colorama  # type: ignore
from dataclasses import dataclass
from typing import List, Dict, Optional, Tuple, cast

import textdistance  # type: ignore

# from asteria.utils import *
from asteria.core.syntax.abstract import *
from asteria.core.semantics.tasks import *


COLOR_ERROR = colorama.Style.BRIGHT + colorama.Fore.RED
COLOR_ERROR_HIGHLIGHT = colorama.Style.BRIGHT + colorama.Fore.RED
COLOR_FILE_NAME = colorama.Fore.RED
COLOR_PATH = colorama.Style.DIM + colorama.Fore.WHITE
COLOR_LINE_NUMBER = colorama.Style.DIM + colorama.Fore.RED
COLOR_DIM = colorama.Style.DIM + colorama.Fore.WHITE
COLOR_HIGHLIGHT = colorama.Style.BRIGHT + colorama.Fore.WHITE
COLOR_RESET = colorama.Style.RESET_ALL


def distance(a, b):
    return textdistance.damerau_levenshtein.distance(a, b)


def extract_and_show_source_lines(first, last, lines):
    return [
        f'  {COLOR_LINE_NUMBER}{lino+1} >{COLOR_RESET}  {COLOR_HIGHLIGHT}{lines[lino]}{COLOR_RESET}' if lino in range(first, last+1)
        else f'  {COLOR_LINE_NUMBER}{lino+1}  {COLOR_RESET}  {COLOR_DIM}{lines[lino]}{COLOR_RESET}'
        for lino in range(max(0, first-2), min(len(lines), last+3))
    ]


@dataclass
class ElabError(BaseException):
    tasks: List[ElaboratorTask]

    def pretty(self) -> str:
        p = self.path()
        fn = p[p.rindex('/')+1:]
        hints = self.hint_lines()
        return '\n'.join(

            [f'{COLOR_ERROR}ERROR{COLOR_RESET} in {COLOR_FILE_NAME}{fn}{COLOR_RESET}',
                f'{COLOR_PATH}{p}{COLOR_RESET}', '']
            + self.message_lines()
            + ['', 'Source context:', '']
            + self.highlight_lines()
            + ([''] + hints if len(hints) > 0 else [])

        )

    def path(self) -> str:
        return 'unknown-file-path'

    def message_lines(self) -> List[str]:
        return ['unknown-error-message']

    def highlight_lines(self) -> List[str]:
        return []

    def hint_lines(self) -> List[str]:
        return []


@dataclass
class MiscError(ElabError):
    lines: List[str]

    def message_lines(self) -> List[str]:
        return self.lines


@dataclass
class TypeConstructorAlreadyDefinedError(ElabError):
    declaration: DataDeclaration

    def path(self):
        return self.declaration.source.source_info['path']

    def message_lines(self) -> List[str]:
        return [
            'Type constructor is already defined:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.declaration.name}{COLOR_RESET}'
        ]

    def highlight_lines(self):
        first_line = self.declaration.source.meta.line-1
        last_line = self.declaration.source.meta.line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.declaration.source.source_info['text_lines'])


@dataclass
class TermConstructorAlreadyDefinedError(ElabError):
    declaration: ConstructorDeclaration

    def path(self):
        return self.declaration.source.source_info['path']

    def message_lines(self):
        return [
            'Constructor is already defined:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.declaration.name}{COLOR_RESET}']

    def highlight_lines(self):
        first_line = self.declaration.source.meta.line-1
        last_line = self.declaration.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.declaration.source.source_info['text_lines'])


@dataclass
class RepeatedParameterInTypeConstructorDeclarationError(ElabError):
    parameter_name: str
    declaration: DataDeclaration

    def path(self):
        return self.declaration.source.source_info['path']

    def message_lines(self):
        return [
            'Repeated parameter in signature for type constructor:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.parameter_name}{COLOR_RESET}'
        ]

    def highlight_lines(self):
        first_line = self.declaration.source.meta.line-1
        last_line = self.declaration.source.meta.line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.declaration.source.source_info['text_lines'])


@dataclass
class RepeatedTypeParameterInTermConstructorDeclarationError(ElabError):
    type_parameter_name: str
    declaration: ConstructorDeclaration

    def path(self):
        return self.declaration.source.source_info['path']

    def message_lines(self):
        return [
            'Repeated type parameter:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.type_parameter_name}{COLOR_RESET}',
            '',
            'In signature for term constructor:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.declaration.name}{COLOR_RESET}'
        ]

    def highlight_lines(self):
        first_line = self.declaration.source.meta.line-1
        last_line = self.declaration.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.declaration.source.source_info['text_lines'])


@dataclass
class RepeatedTermParameterInTermConstructorDeclarationError(ElabError):
    term_parameter_name: str
    declaration: ConstructorDeclaration

    def path(self):
        return self.declaration.source.source_info['path']

    def message_lines(self):
        return [
            'Repeated term parameter:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.term_parameter_name}{COLOR_RESET}',
            '',
            'In signature for term constructor:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.declaration.name}{COLOR_RESET}'
        ]

    def highlight_lines(self):
        first_line = self.declaration.source.meta.line-1
        last_line = self.declaration.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.declaration.source.source_info['text_lines'])


@dataclass
class IncorrectKindForConstructorParameterTypeError(ElabError):
    parameter_name: str
    found_kind: Kind
    declaration: ConstructorDeclaration

    def path(self):
        return self.declaration.source.source_info['path']

    def message_lines(self):
        return [
            'Constructor parameter has an incorrectly kinded type',
            '',
            'Expected kind:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{TypeKind(source=None).pretty()}{COLOR_RESET}',
            '',
            'Found kind:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.found_kind.pretty()}{COLOR_RESET}',
        ]

    def highlight_lines(self):
        first_line = self.declaration.source.meta.line-1
        last_line = self.declaration.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.declaration.source.source_info['text_lines'])


@dataclass
class IncorrectKindForConstructorReturnTypeError(ElabError):
    found_kind: Kind
    declaration: ConstructorDeclaration

    def path(self):
        return self.declaration.source.source_info['path']

    def message_lines(self):
        return [
            'Constructor declaration returns an incorrectly kinded type',
            '',
            'Expected kind:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{TypeKind(source=None).pretty()}{COLOR_RESET}',
            '',
            'Found kind:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.found_kind.pretty()}{COLOR_RESET}',
        ]

    def highlight_lines(self):
        first_line = self.declaration.source.meta.line-1
        last_line = self.declaration.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.declaration.source.source_info['text_lines'])


@dataclass
class IncorrectTypeForConstructorDeclarationReturnTypeError(ElabError):
    found_type: Type
    type_constructor: DeclaredTypeConstructorName
    declaration: ConstructorDeclaration

    def path(self):
        return self.declaration.source.source_info['path']

    def message_lines(self):
        return [
            'Constructor declaration returns an incorrect type',
            '',
            'Expected constructor type of the form:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{ConstructorType(None,self.type_constructor,[MetaSyntacticVariableType(None,"...")]).pretty()}{COLOR_RESET}',
            '',
            'Found type:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.found_type.pretty()}{COLOR_RESET}',
        ]

    def highlight_lines(self):
        first_line = self.declaration.source.meta.line-1
        last_line = self.declaration.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.declaration.source.source_info['text_lines'])


@dataclass
class TermNameAlreadyDefinedError(ElabError):
    declaration: TermDeclaration

    def path(self):
        return self.declaration.source.source_info['path']

    def message_lines(self):
        return [
            'Term name is already defined:',
            '',
            f'   {COLOR_ERROR_HIGHLIGHT}{self.declaration.name.pretty()}{COLOR_RESET}'
        ]

    def highlight_lines(self):
        first_line = self.declaration.source.meta.line-1
        last_line = self.declaration.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.declaration.source.source_info['text_lines'])


@dataclass
class IncorrectKindForTermDeclarationTypeError(ElabError):
    found_kind: Kind
    declaration: TermDeclaration

    def path(self):
        return self.declaration.source.source_info['path']

    def message_lines(self):
        return [
            'Term declaration has an incorrectly kinded type',
            '',
            'Expected kind:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{TypeKind(source=None).pretty()}{COLOR_RESET}',
            '',
            'Found kind:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.found_kind.pretty()}{COLOR_RESET}',
        ]

    def highlight_lines(self):
        first_line = self.declaration.source.meta.line-1
        last_line = self.declaration.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.declaration.source.source_info['text_lines'])


@dataclass
class TypeVariableNotInScopeError(ElabError):
    type: Type
    context: Context

    def path(self):
        return self.type.source.source_info['path']

    def message_lines(self):
        return [
            'Type variable is not in scope:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.type.pretty()}{COLOR_RESET}'
        ]

    def highlight_lines(self):
        first_line = self.type.source.meta.line-1
        last_line = self.type.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.type.source.source_info['text_lines'])

    def hint_lines(self):
        names = [
            v for v, j in self.context.variables.items() if isinstance(j, HasKind)]

        if len(names) == 0:
            return ['No type variables are in scope at that location.']
        else:
            closest_names = sorted(
                names, key=lambda name: distance(self.type.name, name))

            return ['Did you mean one of these?', ''] +\
                [f'  {n} : {self.context.variables[n].kind.pretty()}' for n in closest_names[:10]]


@dataclass
class TermVariableUsedAsTypeVariableError(ElabError):
    type: Type
    context: Context

    def path(self):
        return self.type.source.source_info['path']

    def message_lines(self):
        return [
            'Term variable is used as a type variable:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.type.pretty()}{COLOR_RESET}'
        ]

    def highlight_lines(self):
        first_line = self.type.source.meta.line-1
        last_line = self.type.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.type.source.source_info['text_lines'])

    def hint_lines(self):
        names = [n for n in self.context.variables
                 if isinstance(self.context.variables[n], HasKind)]

        if len(names) == 0:
            return ['No type variables are in scope at that location.']
        else:
            closest_names = sorted(
                names, key=lambda name: distance(self.type.name, name))

            return ['Did you mean one of these?', ''] +\
                [f'  {n} : {self.context.variables[n].kind.pretty()}' for n in closest_names[:10]]


@dataclass
class TypeConstructorNotInScopeError(ElabError):
    type: ConstructorType
    known_type_constructors: Dict[DeclaredTypeConstructorName,
                                  TypeConstructorSignature]

    def path(self):
        return self.type.source.source_info['path']

    def message_lines(self):
        return [
            'Type constructor is not in scope:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.type.name.pretty()}{COLOR_RESET}'
        ]

    def highlight_lines(self):
        first_line = self.type.source.meta.line-1
        last_line = self.type.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.type.source.source_info['text_lines'])

    def hint_lines(self):
        names = self.known_type_constructors.keys()

        if len(names) == 0:
            return ['No type constructors are in scope at that location.']
        else:
            closest_names = sorted(
                names, key=lambda name: distance(self.type.name.name, name.name))

            return ['Did you mean one of these?', ''] +\
                [f'  {n.pretty()} {self.known_type_constructors[n].pretty()}' for n in closest_names[:10]]


@dataclass
class IncorrectKindForTypeParameterError(ElabError):
    type: Type
    parameter: Type
    expected_kind: Kind
    found_kind: Kind

    def path(self):
        return self.type.source.source_info['path']

    def message_lines(self):
        return [
            'Mismatched kinds for type parameter:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.parameter.pretty()}{COLOR_RESET}',
            '',
            'In type:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.type.pretty()}{COLOR_RESET}',
            '',
            'Expected kind:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.expected_kind.pretty()}{COLOR_RESET}',
            '',
            'Found kind:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.found_kind.pretty()}{COLOR_RESET}'
        ]

    def highlight_lines(self):
        first_line = self.type.source.meta.line-1
        last_line = self.type.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.type.source.source_info['text_lines'])


@dataclass
class IncorrectKindForFunctionTypeArgumentTypeError(ElabError):
    type: FunctionType
    found_kind: Kind

    def path(self):
        return self.type.source.source_info['path']

    def message_lines(self):
        return [
            'Mismatched kinds for argument type:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.type.argument_type.pretty()}{COLOR_RESET}',
            '',
            'In function type:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.type.pretty()}{COLOR_RESET}',
            '',
            'Expected kind:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{TypeKind(source=None).pretty()}{COLOR_RESET}',
            '',
            'Found kind:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.found_kind.pretty()}{COLOR_RESET}'
        ]

    def highlight_lines(self):
        first_line = self.type.source.meta.line-1
        last_line = self.type.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.type.source.source_info['text_lines'])


@dataclass
class IncorrectKindForFunctionTypeReturnTypeError(ElabError):
    type: FunctionType
    found_kind: Kind

    def path(self):
        return self.type.source.source_info['path']

    def message_lines(self):
        return [
            'Mismatched kinds for return type:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.type.return_type.pretty()}{COLOR_RESET}',
            '',
            'In function type:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.type.pretty()}{COLOR_RESET}',
            '',
            'Expected kind:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{TypeKind(source=None).pretty()}{COLOR_RESET}',
            '',
            'Found kind:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.found_kind.pretty()}{COLOR_RESET}'
        ]

    def highlight_lines(self):
        first_line = self.type.source.meta.line-1
        last_line = self.type.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.type.source.source_info['text_lines'])


@dataclass
class IncorrectKindForForallTypeScopeError(ElabError):
    type: ForallType
    found_kind: Kind

    def path(self):
        return self.type.source.source_info['path']

    def message_lines(self):
        return [
            'Mismatched kinds for scope:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.type.scope.body.pretty()}{COLOR_RESET}',
            '',
            'In forall type:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.type.pretty()}{COLOR_RESET}',
            '',
            'Expected kind:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{TypeKind(source=None).pretty()}{COLOR_RESET}',
            '',
            'Found kind:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.found_kind.pretty()}{COLOR_RESET}'
        ]

    def highlight_lines(self):
        first_line = self.type.source.meta.line-1
        last_line = self.type.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.type.source.source_info['text_lines'])


@dataclass
class IncorrectKindForApplicationTypeFunctionError(ElabError):
    type: ApplicationType
    found_kind: Kind

    def path(self):
        return self.type.source.source_info['path']

    def message_lines(self):
        return [
            'Mismatched kinds for function:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.type.function.pretty()}{COLOR_RESET}',
            '',
            'In application type:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.type.pretty()}{COLOR_RESET}',
            '',
            'Expected function kind with form:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{FunctionKind(None, MetaSyntacticVariableKind(None, "k1"), MetaSyntacticVariableKind(None, "k2")).pretty()}{COLOR_RESET}',
            '',
            'Found kind:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.found_kind.pretty()}{COLOR_RESET}'
        ]

    def highlight_lines(self):
        first_line = self.type.source.meta.line-1
        last_line = self.type.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.type.source.source_info['text_lines'])


@dataclass
class IncorrectKindForApplicationTypeArgumentError(ElabError):
    type: ApplicationType
    expected_kind: Kind
    found_kind: Kind

    def path(self):
        return self.type.source.source_info['path']

    def message_lines(self):
        return [
            'Mismatched kinds for argument:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.type.argument.pretty()}{COLOR_RESET}',
            '',
            'In application type:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.type.pretty()}{COLOR_RESET}',
            '',
            'Expected kind:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.expected_kind.pretty()}{COLOR_RESET}',
            '',
            'Found kind:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.found_kind.pretty()}{COLOR_RESET}'
        ]

    def highlight_lines(self):
        first_line = self.type.source.meta.line-1
        last_line = self.type.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.type.source.source_info['text_lines'])


@dataclass
class CannotCheckLambdaTermAgainstNonFunctionTypeError(ElabError):
    term: LambdaTerm
    type: Type

    def path(self):
        return self.term.source.source_info['path']

    def message_lines(self):
        return [
            'Cannot check lambda term:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.term.pretty()}{COLOR_RESET}',
            '',
            'Against non-function type:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.type.pretty()}{COLOR_RESET}'
        ]

    def highlight_lines(self):
        first_line = self.term.source.meta.line-1
        last_line = self.term.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.term.source.source_info['text_lines'])


@dataclass
class CannotCheckAbstractionTermAgainstNonForallTypeError(ElabError):
    term: AbstractionTerm
    type: Type

    def path(self):
        return self.term.source.source_info['path']

    def message_lines(self):
        return [
            'Cannot check abstraction term:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.term.pretty()}{COLOR_RESET}',
            '',
            'Against non-forall type:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.type.pretty()}{COLOR_RESET}'
        ]

    def highlight_lines(self):
        first_line = self.term.source.meta.line-1
        last_line = self.term.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.term.source.source_info['text_lines'])


@dataclass
class CannotCheckConstructorTermAgainstNonConstructorTypeError(ElabError):
    term: ConstructorTerm
    type: Type

    def path(self):
        return self.term.source.source_info['path']

    def message_lines(self):
        return [
            'Cannot check constructor term:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.term.pretty()}{COLOR_RESET}',
            '',
            'Against non-constructor type:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.type.pretty()}{COLOR_RESET}'
        ]

    def highlight_lines(self):
        first_line = self.term.source.meta.line-1
        last_line = self.term.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.term.source.source_info['text_lines'])


@dataclass
class UnknownTermConstructorForTypeError(ElabError):
    term: ConstructorTerm
    type: ConstructorType
    known_term_constructors: Dict[str, Scope[ConstructorSignature]]

    def path(self):
        return self.term.source.source_info['path']

    def message_lines(self):
        return [
            'Unknown term constructor:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.term.constructor}{COLOR_RESET}',
            '',
            'For type:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.type.pretty()}{COLOR_RESET}'
        ]

    def highlight_lines(self):
        first_line = self.term.source.meta.line-1
        last_line = self.term.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.term.source.source_info['text_lines'])

    def hint_lines(self):
        names = self.known_term_constructors.keys()

        if len(names) == 0:
            return ['No term constructors are in scope at that location.']
        else:
            closest_names = sorted(
                names, key=lambda name: distance(self.term.constructor, name))

            return ['Did you mean one of these?', ''] +\
                [f'  {n} {self.known_term_constructors[n].body.pretty()}' for n in closest_names[:10]]


@dataclass
class IncorrectNumberOfTypeArgumentsInConstructorTermError(ElabError):
    term: ConstructorTerm
    expected_number: int
    found_number: int

    def path(self):
        return self.term.source.source_info['path']

    def message_lines(self):
        if self.expected_number > self.found_number:
            return [
                'Too few type arguments given in constructor term:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.term.pretty()}{COLOR_RESET}',
                '',
                'Expected number:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.expected_number}{COLOR_RESET}',
                '',
                'Found number:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.found_number}{COLOR_RESET}'
            ]
        else:
            return [
                'Too many type arguments given in constructor term:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.term.pretty()}{COLOR_RESET}',
                '',
                'Expected number:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.expected_number}{COLOR_RESET}',
                '',
                'Found number:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.found_number}{COLOR_RESET}'
            ]

    def highlight_lines(self):
        first_line = self.term.source.meta.line-1
        last_line = self.term.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.term.source.source_info['text_lines'])


@dataclass
class IncorrectKindForTypeArgumentInConstructorTermError(ElabError):
    type_argument: Type
    term: ConstructorTerm
    expected_kind: Kind
    found_kind: Kind

    def path(self):
        return self.type_argument.source.source_info['path']

    def message_lines(self):
        return [
            'Incorrect kind for type argument:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.type_argument.pretty()}{COLOR_RESET}',
            '',
            'In constructor term:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.term.pretty()}{COLOR_RESET}',
            '',
            'Expected kind:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.expected_kind.pretty()}{COLOR_RESET}',
            '',
            'Found kind:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.found_kind.pretty()}{COLOR_RESET}'
        ]

    def highlight_lines(self):
        first_line = self.type_argument.source.meta.line-1
        last_line = self.type_argument.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.type_argument.source.source_info['text_lines'])


@dataclass
class IncorrectKindForTypeArgumentInConstructorPatternError(ElabError):
    type_argument: Type
    pattern: ConstructorPattern
    expected_kind: Kind
    found_kind: Kind

    def path(self):
        return self.type_argument.source.source_info['path']

    def message_lines(self):
        return [
            'Incorrect kind for type argument:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.type_argument.pretty()}{COLOR_RESET}',
            '',
            'In constructor pattern:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.pattern.pretty()}{COLOR_RESET}',
            '',
            'Expected kind:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.expected_kind.pretty()}{COLOR_RESET}',
            '',
            'Found kind:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.found_kind.pretty()}{COLOR_RESET}'
        ]

    def highlight_lines(self):
        first_line = self.type_argument.source.meta.line-1
        last_line = self.type_argument.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.type_argument.source.source_info['text_lines'])


@dataclass
class IncorrectNumberOfArgumentsInConstructorTermError(ElabError):
    term: ConstructorTerm
    expected_number: int
    found_number: int

    def path(self):
        return self.term.source.source_info['path']

    def message_lines(self):
        if self.expected_number > self.found_number:
            return [
                'Too few arguments given in constructor term:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.term.pretty()}{COLOR_RESET}',
                '',
                'Expected number:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.expected_number}{COLOR_RESET}',
                '',
                'Found number:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.found_number}{COLOR_RESET}'
            ]
        else:
            return [
                'Too many arguments given in constructor term:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.term.pretty()}{COLOR_RESET}',
                '',
                'Expected number:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.expected_number}{COLOR_RESET}',
                '',
                'Found number:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.found_number}{COLOR_RESET}'
            ]

    def highlight_lines(self):
        first_line = self.term.source.meta.line-1
        last_line = self.term.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.term.source.source_info['text_lines'])


@dataclass
class ConstructorTermDoesNotInhabitCheckTypeError(ElabError):
    term: ConstructorTerm
    expected_type: Type
    found_type: Type

    def path(self):
        return self.term.source.source_info['path']

    def message_lines(self):
        return [
            'Constructor term does not inhabit the type it\'s being checked against:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.term.pretty()}{COLOR_RESET}',
            '',
            'Expected type:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.expected_type.pretty()}{COLOR_RESET}',
            '',
            'Found type:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.found_type.pretty()}{COLOR_RESET}'
        ]

    def highlight_lines(self):
        first_line = self.term.source.meta.line-1
        last_line = self.term.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.term.source.source_info['text_lines'])


@dataclass
class IncorrectTypeForSynthesizedTermError(ElabError):
    term: Term
    expected_type: Type
    found_type: Type

    def path(self):
        return self.term.source.source_info['path']

    def message_lines(self):
        return [
            'Incorrect type when checking synthesizable term:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.term.pretty()}{COLOR_RESET}',
            '',
            'Expected type:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.expected_type.pretty()}{COLOR_RESET}',
            '',
            'Found type:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.found_type.pretty()}{COLOR_RESET}'
        ]

    def highlight_lines(self):
        first_line = self.term.source.meta.line-1
        last_line = self.term.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.term.source.source_info['text_lines'])


@dataclass
class VariableNotInScopeError(ElabError):
    term: VariableTerm
    context: Context

    def path(self):
        return self.term.source.source_info['path']

    def message_lines(self):
        return [
            'Variable is not in scope:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.term.name}{COLOR_RESET}'
        ]

    def highlight_lines(self):
        first_line = self.term.source.meta.line-1
        last_line = self.term.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.term.source.source_info['text_lines'])

    def hint_lines(self):
        names = [n for n in self.context.variables
                 if isinstance(self.context.variables[n], HasType)]

        if len(names) == 0:
            return ['No term variables are in scope at that location.']
        else:
            closest_names = sorted(
                names, key=lambda name: distance(self.term.name, name))

            return ['Did you mean one of these?', ''] +\
                [f'  {n} : {self.context.variables[n].type.pretty()}' for n in closest_names[:10]]


@dataclass
class TypeVariableUsedAsTermVariableError(ElabError):
    term: VariableTerm
    context: Context

    def path(self):
        return self.term.source.source_info['path']

    def message_lines(self):
        return [
            'Type variable is used as term variable:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.term.name}{COLOR_RESET}'
        ]

    def highlight_lines(self):
        first_line = self.term.source.meta.line-1
        last_line = self.term.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.term.source.source_info['text_lines'])

    def hint_lines(self):
        names = [n for n in self.context.variables
                 if isinstance(self.context.variables[n], HasType)]

        if len(names) == 0:
            return ['No term variables are in scope at that location.']
        else:
            closest_names = sorted(
                names, key=lambda name: distance(self.term.name, name))

            return ['Did you mean one of these?', ''] +\
                [f'  {n} : {self.context.variables[n].type.pretty()}' for n in closest_names[:10]]


@dataclass
class DeclaredTermNameNotInScopeError(ElabError):
    term: DeclaredTermNameTerm
    known_term_names: Dict[DeclaredTermName, Type]

    def path(self):
        return self.term.source.source_info['path']

    def message_lines(self):
        return [
            'Declared term name is not in scope:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.term.name.pretty()}{COLOR_RESET}'
        ]

    def highlight_lines(self):
        first_line = self.term.source.meta.line-1
        last_line = self.term.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.term.source.source_info['text_lines'])

    def hint_lines(self):
        names = self.known_term_names.keys()

        if len(names) == 0:
            return ['No declared term names are in scope at that location.']
        else:
            closest_names = sorted(
                names, key=lambda name: distance(self.term.name.pretty(), name.pretty()))

            return ['Did you mean one of these?', ''] +\
                [f'  {n.pretty()} : {self.known_term_names[n].pretty()}' for n in closest_names[:10]]


@dataclass
class IncorrectKindForAnnotationTypeError(ElabError):
    term: Term
    found_kind: Kind

    def path(self):
        return self.term.type.source.source_info['path']

    def message_lines(self):
        return [
            'Incorrect kind when checking the type:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.term.type.pretty()}{COLOR_RESET}',
            '',
            'In the annotation term:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.term.pretty()}{COLOR_RESET}',
            '',
            'Expected kind:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{TypeKind(source=None).pretty()}{COLOR_RESET}',
            '',
            'Found kind:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.found_kind.pretty()}{COLOR_RESET}'
        ]

    def highlight_lines(self):
        first_line = self.term.source.meta.line-1
        last_line = self.term.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.term.source.source_info['text_lines'])


@dataclass
class CannotSynthesizeTypeForLambdaTermError(ElabError):
    term: LambdaTerm

    def path(self):
        return self.term.source.source_info['path']

    def message_lines(self):
        return [
            'Cannot synthesize type for lambda term:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.term.pretty()}{COLOR_RESET}',
        ]

    def highlight_lines(self):
        first_line = self.term.source.meta.line-1
        last_line = self.term.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.term.source.source_info['text_lines'])

    def hint_lines(self):
        return [
            'Try annotating the lambda term with a function type, like so:',
            '',
            f'  ({self.term.pretty()}) : {FunctionType(None,MetaSyntacticVariableType(None,"..."),MetaSyntacticVariableType(None,"...")).pretty()}'
        ]


@dataclass
class IncorrectTypeForApplicationTermFunctionError(ElabError):
    term: ApplicationTerm
    found_type: Type

    def path(self):
        return self.term.source.source_info['path']

    def message_lines(self):
        return [
            'Incorrect type for the function part:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.term.function.pretty()}{COLOR_RESET}',
            '',
            'In the application term:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.term.pretty()}{COLOR_RESET}',
            '',
            'Expected function type of form:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{FunctionType(None, MetaSyntacticVariableType(None, "..."), MetaSyntacticVariableType(None, "...")).pretty()}{COLOR_RESET}',
            '',
            'Found type:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.found_type.pretty()}{COLOR_RESET}'
        ]

    def highlight_lines(self):
        first_line = self.term.source.meta.line-1
        last_line = self.term.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.term.source.source_info['text_lines'])


@dataclass
class CannotSynthesizeTypeForAbstractionTermError(ElabError):
    term: AbstractionTerm

    def path(self):
        return self.term.source.source_info['path']

    def message_lines(self):
        return [
            'Cannot synthesize type for abstraction term:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.term.pretty()}{COLOR_RESET}',
        ]

    def highlight_lines(self):
        first_line = self.term.source.meta.line-1
        last_line = self.term.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.term.source.source_info['text_lines'])

    def hint_lines(self):
        return [
            'Try annotating the abstraction term with a forall type, like so:',
            '',
            f'  ({self.term.pretty()}) : {ForallType(None,MetaSyntacticVariableKind(None,"..."),Scope(self.term.body.names,MetaSyntacticVariableType(None,"..."))).pretty()}'
        ]


@dataclass
class IncorrectTypeForInstantiationTermFunctionError(ElabError):
    term: InstantiationTerm
    found_type: Type

    def path(self):
        return self.term.source.source_info['path']

    def message_lines(self):
        return [
            'Incorrect type for the function part:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.term.function.pretty()}{COLOR_RESET}',
            '',
            'In the instantiation term:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.term.pretty()}{COLOR_RESET}',
            '',
            'Expected forall type of form:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{ForallType(None, MetaSyntacticVariableKind(None,"..."), Scope(["a"], MetaSyntacticVariableType(None, "..."))).pretty()}{COLOR_RESET}',
            '',
            'Found type:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.found_type.pretty()}{COLOR_RESET}'
        ]

    def highlight_lines(self):
        first_line = self.term.source.meta.line-1
        last_line = self.term.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.term.source.source_info['text_lines'])


@dataclass
class IncorrectKindForInstantiationTermArgumentError(ElabError):
    term: InstantiationTerm
    expected_kind: Kind
    found_kind: Kind

    def path(self):
        return self.term.source.source_info['path']

    def message_lines(self):
        return [
            'Mismatched kinds for argument:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.term.argument.pretty()}{COLOR_RESET}',
            '',
            'In instantiation term:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.term.pretty()}{COLOR_RESET}',
            '',
            'Expected kind:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.expected_kind.pretty()}{COLOR_RESET}',
            '',
            'Found kind:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.found_kind.pretty()}{COLOR_RESET}'
        ]

    def highlight_lines(self):
        first_line = self.term.source.meta.line-1
        last_line = self.term.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.term.source.source_info['text_lines'])


@dataclass
class CannotSynthesizeTypeForConstructorTermError(ElabError):
    term: ConstructorTerm

    def path(self):
        return self.term.source.source_info['path']

    def message_lines(self):
        return [
            'Cannot synthesize type for constructor term:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.term.pretty()}{COLOR_RESET}',
        ]

    def highlight_lines(self):
        first_line = self.term.source.meta.line-1
        last_line = self.term.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.term.source.source_info['text_lines'])

    def hint_lines(self):
        return [
            'Try annotating the constructor term with a constructor type, like so:',
            '',
            f'  {self.term.pretty()} : {ConstructorType(None, DeclaredTypeConstructorName(["..."], "..."), [MetaSyntacticVariableType(None,"...")]).pretty()}'
        ]


@dataclass
class CannotSynthesizeTypeForEmptyCaseTermError(ElabError):
    term: CaseTerm

    def path(self):
        return self.term.source.source_info['path']

    def message_lines(self):
        return [
            'Cannot synthesize type for empty case term:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.term.pretty()}{COLOR_RESET}',
        ]

    def highlight_lines(self):
        first_line = self.term.source.meta.line-1
        last_line = self.term.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.term.source.source_info['text_lines'])


@dataclass
class DifferentTypesForCaseClausesError(ElabError):
    term: CaseTerm
    clauses: List[Tuple[CaseClause, Type]]

    def path(self):
        return self.term.source.source_info['path']

    def message_lines(self):
        return [
            'Different types for the clauses of the case term:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.term.pretty()}{COLOR_RESET}',
        ] + [
            ln
            for cls, ty in self.clauses
            for ln in [
                '',
                'Case clause:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{cls.pretty()}{COLOR_RESET}',
                '',
                'Has type:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{ty.pretty()}{COLOR_RESET}'
            ]
        ]

    def highlight_lines(self):
        first_line = self.term.source.meta.line-1
        last_line = self.term.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.term.source.source_info['text_lines'])


@dataclass
class RepeatedVariablesInCaseClausePatternsError(ElabError):
    clause: CaseClause
    repeated_variables: List[str]

    def path(self):
        return self.clause.source.source_info['path']

    def message_lines(self):
        return [
            'Found repeated variables:',
            '',
            f'  {", ".join(self.repeated_variables)}',
            '',
            'In case clause:',
            '',
            f'  {self.clause.pretty()}',
        ]

    def highlight_lines(self):
        first_line = self.clause.source.meta.line-1
        last_line = self.clause.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.clause.source.source_info['text_lines'])


@dataclass
class IncorrectNumberOfPatternsInCaseClauseError(ElabError):
    clause: CaseClause
    expected_number: int
    found_number: int

    def path(self):
        return self.clause.source.source_info['path']

    def message_lines(self):
        if self.expected_number > self.found_number:
            return [
                'Too few patterns given in case clause:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.clause.pretty()}{COLOR_RESET}',
                '',
                'Expected number:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.expected_number}{COLOR_RESET}',
                '',
                'Found number:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.found_number}{COLOR_RESET}'
            ]
        else:
            return [
                'Too many patterns given in case clause:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.clause.pretty()}{COLOR_RESET}',
                '',
                'Expected number:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.expected_number}{COLOR_RESET}',
                '',
                'Found number:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.found_number}{COLOR_RESET}'
            ]

    def highlight_lines(self):
        first_line = self.clause.source.meta.line-1
        last_line = self.clause.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.clause.source.source_info['text_lines'])


@dataclass
class UnknownTermConstructorForTypePatternError(ElabError):
    pattern: ConstructorPattern
    type: ConstructorType
    known_term_constructors: Dict[str, Scope[ConstructorSignature]]

    def path(self):
        return self.pattern.source.source_info['path']

    def message_lines(self):
        return [
            'Unknown term constructor:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.pattern.constructor}{COLOR_RESET}',
            '',
            'For type:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.type.pretty()}{COLOR_RESET}'
        ]

    def highlight_lines(self):
        first_line = self.pattern.source.meta.line-1
        last_line = self.pattern.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.pattern.source.source_info['text_lines'])

    def hint_lines(self):
        names = self.known_term_constructors.keys()

        if len(names) == 0:
            return ['No term constructors are in scope at that location.']
        else:
            closest_names = sorted(
                names, key=lambda name: distance(self.pattern.constructor, name))

            return ['Did you mean one of these?', ''] +\
                [f'  {n} {self.known_term_constructors[n].body.pretty()}' for n in closest_names[:10]]


@dataclass
class IncorrectNumberOfTypeArgumentsInConstructorPatternError(ElabError):
    pattern: ConstructorPattern
    expected_number: int
    found_number: int

    def path(self):
        return self.pattern.source.source_info['path']

    def message_lines(self):
        if self.expected_number > self.found_number:
            return [
                'Too few type arguments given in constructor pattern:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.pattern.pretty()}{COLOR_RESET}',
                '',
                'Expected number:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.expected_number}{COLOR_RESET}',
                '',
                'Found number:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.found_number}{COLOR_RESET}'
            ]
        else:
            return [
                'Too many type arguments given in constructor pattern:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.pattern.pretty()}{COLOR_RESET}',
                '',
                'Expected number:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.expected_number}{COLOR_RESET}',
                '',
                'Found number:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.found_number}{COLOR_RESET}'
            ]

    def highlight_lines(self):
        first_line = self.pattern.source.meta.line-1
        last_line = self.pattern.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.pattern.source.source_info['text_lines'])


@dataclass
class IncorrectNumberOfArgumentsInConstructorPatternError(ElabError):
    pattern: ConstructorPattern
    expected_number: int
    found_number: int

    def path(self):
        return self.pattern.source.source_info['path']

    def message_lines(self):
        if self.expected_number > self.found_number:
            return [
                'Too few arguments given in constructor pattern:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.pattern.pretty()}{COLOR_RESET}',
                '',
                'Expected number:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.expected_number}{COLOR_RESET}',
                '',
                'Found number:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.found_number}{COLOR_RESET}'
            ]
        else:
            return [
                'Too many arguments given in constructor pattern:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.pattern.pretty()}{COLOR_RESET}',
                '',
                'Expected number:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.expected_number}{COLOR_RESET}',
                '',
                'Found number:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.found_number}{COLOR_RESET}'
            ]

    def highlight_lines(self):
        first_line = self.pattern.source.meta.line-1
        last_line = self.pattern.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.pattern.source.source_info['text_lines'])


@dataclass
class ConstructorPatternDoesNotInhabitCheckTypeError(ElabError):
    pattern: ConstructorPattern
    expected_type: Type
    found_type: Type

    def path(self):
        return self.pattern.source.source_info['path']

    def message_lines(self):
        return [
            'Constructor pattern does not inhabit the type it\'s being checked against:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.pattern.pretty()}{COLOR_RESET}',
            '',
            'Expected type:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.expected_type.pretty()}{COLOR_RESET}',
            '',
            'Found type:',
            '',
            f'  {COLOR_ERROR_HIGHLIGHT}{self.found_type.pretty()}{COLOR_RESET}'
        ]

    def highlight_lines(self):
        first_line = self.pattern.source.meta.line-1
        last_line = self.pattern.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.pattern.source.source_info['text_lines'])


@dataclass
class TypeParameterNotUniformInConstructors(ElabError):
    declaration: ConstructorDeclaration
    argument: Type
    expected_parameter: str

    def path(self):
        return self.argument.source.source_info['path']

    def message_lines(self):
        if isinstance(self.argument, VariableType) and self.argument.name == self.expected_parameter:
            return [
                'Non-uniform parameter argument:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.argument.pretty()} (coming from the constructor\'s type arguments){COLOR_RESET}',
                '',
                'In declaration for constructor:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.declaration.name}{COLOR_RESET}',
                '',
                'Expected type:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{VariableType(None,self.expected_parameter).pretty()} (coming from the type\'s parameters){COLOR_RESET}'
            ]
        else:
            return [
                'Non-uniform parameter argument:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.argument.pretty()}{COLOR_RESET}',
                '',
                'In declaration for constructor:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{self.declaration.name}{COLOR_RESET}',
                '',
                'Expected type:',
                '',
                f'  {COLOR_ERROR_HIGHLIGHT}{VariableType(None,self.expected_parameter).pretty()}{COLOR_RESET}'
            ]

    def highlight_lines(self):
        first_line = self.declaration.source.meta.line-1
        last_line = self.declaration.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.declaration.source.source_info['text_lines'])

    def hint_lines(self):
        msg = [
            'A type constructor argument that is declared to be a parameter like so:',
            '',
            f'  {ParameterArgument(None,"a",TypeKind(None)).pretty()}',
            '',
            'must be used uniformly, as a type variable, in all the return types for the',
            'constructors of that type.',
            '',
            'If the argument should vary from constructor to constructor, then the argument',
            'should be declared to be an index, like so:',
            '',
            f'  {IndexArgument(None,"a",TypeKind(None)).pretty()}'
        ]

        if isinstance(self.argument, VariableType) and self.argument.name == self.expected_parameter:
            msg += [
                '',
                'The parameter argument\'s name was shadowed by a type argument to the',
                'constructor, leading to non-uniformity. Was this intentional?'
            ]

        return msg
