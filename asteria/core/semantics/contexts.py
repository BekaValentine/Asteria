from dataclasses import dataclass
from typing import List, Dict, Optional, Tuple, cast

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
