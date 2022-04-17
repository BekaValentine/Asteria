import lark
from pathlib import Path

import asteria.core.syntax.concrete as concrete
import asteria.core.syntax.abstract as abstract


path = Path(__file__).parent / 'test.asteriacore'
with path.open() as f:
    src = f.read()

cst = concrete.parse(path, src)
print(cst.pretty())
print(abstract.Module_from_cst(cst))
