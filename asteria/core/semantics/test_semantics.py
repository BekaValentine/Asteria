import lark
from pathlib import Path

import asteria.core.syntax.concrete as concrete
import asteria.core.syntax.abstract as abstract
import asteria.core.semantics.static as static

path = Path(__file__).parent.parent / 'syntax' / 'test.asteriacore'
with path.open() as f:
    src = f.read()

cst = concrete.parse(src)
# print(cst.pretty())

mod = abstract.Module_from_cst(cst)
# for decl in mod.declarations:
#     print()
#     print(decl)
elaborator = static.Elaborator()
elaborator.elab_module(['Test'], mod)
