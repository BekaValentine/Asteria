from dataclasses import dataclass, fields
import re
from typing import Dict, List, TypeVar, Generic, Tuple, Optional, Union, ClassVar, cast
import lark


def camel_to_snake(s):
    return re.sub('((.)([A-Z]))', '\\2_\\3', s).lower()


def todo():
    raise ValueError("Todo")


def flag(s):
    print(f'!!!!\n!!!! {s}\n!!!!')


def fresh_variable(old: List[str], name: str):
    index = None
    proposed = name
    while proposed in old:
        if index is None:
            index = 0
        else:
            index += 1
        proposed = f'{name}_{index}'
    return proposed


def fresh_variables(olds: List[str], names: List[str]) -> List[str]:
    news = []
    for name in names:
        new = fresh_variable(olds, name)
        olds.append(new)
        news.append(new)
    return news


@dataclass(eq=False)
class Syntax(object):
    paren: ClassVar[List[str]] = []
    source: Optional[lark.Tree]

    def __eq__(self, other) -> bool:
        return type(self) == type(other) and all([
            getattr(self, field.name) == getattr(other, field.name)
            for field in fields(self)
            if field.name != 'source'
        ])

    @staticmethod
    def pretty_value(obj, loc):
        if isinstance(obj, Syntax):
            return obj.pretty(loc)
        elif isinstance(obj, Scope):
            return (obj.names, obj.body.pretty(loc))
        elif callable(getattr(obj, 'pretty', None)):
            return obj.pretty()
        elif isinstance(obj, str):
            return obj
        elif isinstance(obj, list):
            return [Syntax.pretty_value(x, loc) for x in obj]
        elif isinstance(obj, tuple):
            return tuple(Syntax.pretty_value(list(obj), loc))
        elif isinstance(obj, object):
            return {
                k: Syntax.pretty_value(obj[k], loc)
                for k in obj
            }
        else:
            return repr(part)

    def pretty(self, loc=None) -> str:
        construct_name = camel_to_snake(self.__class__.__name__)
        parts: List[Union[str, List[str], Tuple[List[str], str]]] = []
        for partloc, part in self.__dict__.items():
            if partloc != 'source':
                parts.append(Syntax.pretty_value(
                    part, f'{construct_name}.{partloc}'))

        p = self.concrete(*parts)
        if loc in self.paren:
            p = f'({p})'
        return p

    def concrete(self, *args) -> str:
        return f'(unknown-syntax {self.__class__.__name__})'

    def rename(self, renaming):
        if renaming == {}:
            return self
        return type(self)(**{field.name: Syntax.rename_value(getattr(self, field.name), renaming) for field in fields(self)})

    @staticmethod
    def rename_value(x, renaming):
        if isinstance(x, Syntax):
            return x.rename(renaming)
        elif isinstance(x, Scope):
            return x.rename(renaming)
        elif isinstance(x, list):
            return [Syntax.rename_value(y, renaming) for y in x]
        elif isinstance(x, tuple):
            return tuple(Syntax.rename_value(list(x), renaming))
        else:
            # flag('Falling through')
            # flag(x)
            return x


T = TypeVar('T')


@dataclass
class Scope(Generic[T]):
    names: List[str]
    body: T

    def rename(self, renaming: Dict[str, str]):
        if renaming == {}:
            return self
        return Scope(self.names, cast(T, cast(Syntax, self.body).rename({k: v for k, v in renaming.items() if k not in self.names})))

    def open(self, free_names: List[str]) -> Tuple[List[str], T]:
        new_names = fresh_variables(free_names, self.names)
        return (new_names, cast(T, cast(Syntax, self.body).rename(dict(zip(self.names, new_names)))))
