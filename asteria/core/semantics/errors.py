import colorama  # type: ignore
from dataclasses import dataclass
from typing import List, Dict, Optional, Tuple, cast

# from asteria.utils import *
from asteria.core.syntax.abstract import *
from asteria.core.semantics.tasks import *


COLOR_ERROR = colorama.Style.BRIGHT + colorama.Fore.RED
COLOR_NAME = colorama.Style.BRIGHT + colorama.Fore.CYAN
COLOR_LINE_NUMBER = colorama.Style.DIM + colorama.Fore.RED
COLOR_DIM = colorama.Style.DIM + colorama.Fore.WHITE
COLOR_HIGHLIGHT = colorama.Style.BRIGHT + colorama.Fore.WHITE
COLOR_RESET = colorama.Style.RESET_ALL


def extract_and_show_source_lines(first, last, lines):
    return [
        f'{COLOR_LINE_NUMBER}{lino} >{COLOR_RESET}  {COLOR_HIGHLIGHT}{lines[lino]}{COLOR_RESET}' if lino in range(first, last+1)
        else f'{COLOR_LINE_NUMBER}{lino}  {COLOR_RESET}  {COLOR_DIM}{lines[lino]}{COLOR_RESET}'
        for lino in range(max(0, first-2), min(len(lines), last+3))
    ]


@dataclass
class ElabError(BaseException):
    tasks: List[ElaboratorTask]

    def pretty(self) -> str:
        return '\n'.join(

            [f'{COLOR_ERROR}ERROR{COLOR_RESET} in {self.path()}', '']
            + self.message_lines()
            + ['']
            + self.highlight_lines()

        )

    def path(self) -> str:
        return 'unknown-file-path'

    def message_lines(self) -> List[str]:
        return ['unknown-error-message']

    def highlight_lines(self) -> List[str]:
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
        return [f'Type constructor already defined']

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
        return [f'Constructor {COLOR_NAME}{self.declaration.name}{COLOR_RESET} is already defined']

    def highlight_lines(self):
        first_line = self.declaration.source.meta.line-1
        last_line = self.declaration.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.declaration.source.source_info['text_lines'])


@dataclass
class RepeatedTypeConstructorParameterError(ElabError):
    parameter_name: str
    declaration: DataDeclaration

    def path(self):
        return self.declaration.source.source_info['path']

    def message_lines(self):
        return [f'Repeated parameter {COLOR_NAME}{self.parameter_name}{COLOR_RESET} in signature for type constructor']

    def highlight_lines(self):
        first_line = self.declaration.source.meta.line-1
        last_line = self.declaration.source.meta.line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.declaration.source.source_info['text_lines'])


@dataclass
class RepeatedTermConstructorTypeParameterError(ElabError):
    type_parameter_name: str
    declaration: ConstructorDeclaration

    def path(self):
        return self.declaration.source.source_info['path']

    def message_lines(self):
        return [f'Repeated type parameter {COLOR_NAME}{self.type_parameter_name}{COLOR_RESET} in signature for term constructor {COLOR_NAME}{self.declaration.name}{COLOR_RESET}']

    def highlight_lines(self):
        first_line = self.declaration.source.meta.line-1
        last_line = self.declaration.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.declaration.source.source_info['text_lines'])


@dataclass
class RepeatedTermParameterInTermConstructorDeclError(ElabError):
    term_parameter_name: str
    declaration: ConstructorDeclaration

    def path(self):
        return self.declaration.source.source_info['path']

    def message_lines(self):
        return [f'Repeated term parameter {COLOR_NAME}{self.term_parameter_name}{COLOR_RESET} in signature for term constructor {COLOR_NAME}{self.declaration.name}{COLOR_RESET}']

    def highlight_lines(self):
        first_line = self.declaration.source.meta.line-1
        last_line = self.declaration.source.meta.end_line-1
        return extract_and_show_source_lines(first_line, last_line,
                                             self.declaration.source.source_info['text_lines'])
