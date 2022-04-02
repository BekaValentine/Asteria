import lark
from pathlib import Path

import asteria.syntax.concrete.core as core_parser
import asteria.syntax.abstract.core as core


if __name__ == '__main__':
    path = Path(__file__).parent / 'concrete/test.asteriacore'
    with path.open() as f:
        src = f.read()

    cst = core_parser.parse(src)
    print(cst.pretty())
