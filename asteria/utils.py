from dataclasses import dataclass, fields
from typing import List


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


def fresh_variables(olds: List[str], names: List[str]):
    news = []
    for name in names:
        new = fresh_variable(olds, name)
        olds.append(new)
        news.append(new)
    return news


@dataclass
class Syntax(object):

    def pretty(self, prec=None) -> str:
        return f'(unknown-syntax {self})'

    def rename(self, renaming):
        if renaming == {}:
            return self
        return type(self)(**{field.name: Syntax.rename_value(getattr(self, field.name), renaming) for field in fields(self)})

    @staticmethod
    def rename_value(x, renaming):
        if isinstance(x, Syntax):
            return x.rename(renaming)
        elif isinstance(x, list):
            return [Syntax.rename_value(y, renaming) for y in x]
        else:
            return x


# @dataclass
# class Variable(Syntax):
#     name: object
#
#     def pretty(self, prec=None) -> str:
#         return f'{self.name}'
#
#     def rename(self, renaming):
#         if renaming == {}:
#             return self
#         if self.name in renaming:
#             return Variable(renaming[self.name])
#         else:
#             return self


@dataclass
class Scope(Syntax):
    names: List[str]
    body: Syntax

    def rename(self, renaming):
        if renaming == {}:
            return self
        return Scope(self.names, self.body.rename({k: v for k, v in renaming.items() if k not in self.names}))

    def open(self, free_names):
        new_names = fresh_variables(free_names, self.names)
        return (new_names, self.body.rename(dict(zip(self.names, new_names))))
