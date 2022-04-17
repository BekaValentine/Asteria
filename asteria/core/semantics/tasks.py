from dataclasses import dataclass
from typing import List, Dict, Optional, Tuple, cast

from asteria.core.syntax.abstract import *
from asteria.core.semantics.contexts import *


@dataclass
class ElaboratorTask(object):
    pass


@dataclass
class ElabModule(ElaboratorTask):
    name: List[str]
    module: Module


@dataclass
class ElabDataDeclaration(ElaboratorTask):
    declaration: DataDeclaration


@dataclass
class ElabConstructorDeclaration(ElaboratorTask):
    declaration: ConstructorDeclaration


@dataclass
class ElabTermDeclaration(ElaboratorTask):
    declaration: TermDeclaration


@dataclass
class SynthType(ElaboratorTask):
    context: Context
    type: Type


@dataclass
class CheckTerm(ElaboratorTask):
    context: Context
    type: Type
    term: Term


@dataclass
class SynthTerm(ElaboratorTask):
    context: Context
    term: Term


@dataclass
class SynthClause(ElaboratorTask):
    context: Context
    pattern_types: List[Type]
    clause: CaseClause


@dataclass
class CheckPattern(ElaboratorTask):
    context: Context
    type: Type
    pattern: Pattern
