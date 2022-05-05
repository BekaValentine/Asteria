import lark
from pathlib import Path

import asteria.core.syntax.concrete as concrete
import asteria.core.syntax.abstract as abstract
import asteria.core.semantics.static as static
import asteria.core.semantics.errors as errors

path = Path(__file__).parent.parent / 'syntax' / 'test.asteriacore'
with path.open() as f:
    src = f.read()

cst = concrete.parse(path, src)

mod = abstract.Module_from_cst(cst)
elaborator = static.Elaborator()
elaborator.verbose = True

try:
    elaborator.elab_module(['Test'], mod)
except errors.ElabError as e:
    print(e.pretty())
