import lark
from pathlib import Path

import asteria.syntax.concrete.parser as parser
import asteria.syntax.abstract.surface as surface


if __name__ == '__main__':
    path = Path(__file__).parent / 'concrete/test.asteria'
    with path.open() as f:
        src = f.read()

    cst = parser.parse(src)
    print(cst.pretty())
    print(surface.Module_from_cst(cst))
