import lark
import os
from pathlib import Path
import pytest
import sys

import asteria.utils as utils
import asteria.core.syntax.concrete as concrete
import asteria.core.syntax.abstract as abstract
import asteria.core.semantics.static as static
import asteria.core.semantics.errors as errors


args = sys.argv[1:]

if args[0] == 'errors':

    files_to_test = []

    if len(args) == 1:

        prefix_length = len(str(Path(__file__).parent))+1
        errors_path = Path(__file__).parent / 'errors'
        first = True
        for dir_path, dirs, files in os.walk(errors_path):

            for file in sorted(files):
                if file[-len('.asteriacore'):] == '.asteriacore':
                    files_to_test.append((dir_path, file))

    else:

        for arg in args[1:]:
            if '/' in arg:
                i = arg.rindex('/')
                files_to_test.append((arg[:i], arg[i+1:]))
            else:
                files_to_test.append(('.', arg))

    # Testing Errors

    print()
    print('ERROR TESTS ' + 68*'=')

    # prefix_length = len(str(Path(__file__).parent))+1
    # errors_path = Path(__file__).parent / 'errors'
    # first = True
    # for dir_path, dirs, files in os.walk(errors_path):
    #
    #     for file in sorted(files):
    #         if file[-len('.asteriacore'):] == '.asteriacore':

    first = True
    for dir_path, file in files_to_test:

        test_path = Path(dir_path) / file

        print()

        if not os.path.exists(test_path):
            print(f'Invalid file path: {test_path}')

            continue

        if first:
            first = False
        else:
            print(80*'-')
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
