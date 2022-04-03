import lark
from pathlib import Path

import asteria.syntax.surface.concrete as concrete
import asteria.syntax.surface.abstract as abstract


if __name__ == '__main__':
    path = Path(__file__).parent / 'test.asteria'
    with path.open() as f:
        src = f.read()

    cst = concrete.parse(src)
    print(cst.pretty())
    print(abstract.Module_from_cst(cst))
