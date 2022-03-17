import lark
from pathlib import Path

import concrete.parser as parser
import abstract.ast as ast


if __name__ == '__main__':
    path = Path(__file__).parent / 'concrete/test.asteria'
    with path.open() as f:
        src = f.read()

    cst = parser.parse(src)
    print(cst.pretty())
    print(ast.Module.from_cst(cst))
