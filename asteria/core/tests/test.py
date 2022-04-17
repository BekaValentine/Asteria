import lark
import os
from pathlib import Path
import pytest

import asteria.utils as utils
import asteria.core.syntax.concrete as concrete
import asteria.core.syntax.abstract as abstract
import asteria.core.semantics.static as static
import asteria.core.semantics.errors as errors


# Testing Errors

prefix_length = len(str(Path(__file__).parent))+1
errors_path = Path(__file__).parent / 'errors'
for dir_path, dirs, files in os.walk(errors_path):

    for file in files:
        if file[-len('.asteriacore'):] == '.asteriacore':

            test_path = Path(dir_path) / file

            print()
            with open(test_path) as f:
                src = f.read()

            cst = concrete.parse(test_path, src)

            mod = abstract.Module_from_cst(cst)

            elaborator = static.Elaborator()

            if __name__ == '__main__':
                assert abstract.TypeKind(None) == abstract.TypeKind(cst)
                try:
                    elaborator.elab_module(['Test'], mod)

                except errors.ElabError as err:
                    print(err.pretty())

            else:

                with pytest.raises(errors.ElabError):

                    elaborator.elab_module(['Test'], mod)

    print()
