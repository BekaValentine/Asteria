import lark
from pathlib import Path

import asteria.surface.syntax.concrete as concrete
import asteria.surface.syntax.abstract as abstract


if __name__ == '__main__':
    path = Path(__file__).parent / 'test.asteria'
    with path.open() as f:
        src = f.read()

    cst = concrete.parse(src)
    print(cst.pretty())
    print(abstract.Module_from_cst(cst))
