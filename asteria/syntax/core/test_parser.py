import lark
from pathlib import Path

import asteria.syntax.core.concrete as concrete
import asteria.syntax.core.abstract as abstract


if __name__ == '__main__':
    path = Path(__file__).parent / 'test.asteriacore'
    with path.open() as f:
        src = f.read()

    cst = concrete.parse(src)
    print(cst.pretty())
