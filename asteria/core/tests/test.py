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

    files = []
    show = False

    if len(args) > 1 and args[1] == '--show':
        files = args[2:]
        show = True

    if len(files) == 0:

        prefix_length = len(str(Path(__file__).parent))+1
        errors_path = Path(__file__).parent / 'errors'
        first = True
        for dir_path, dirs, files in os.walk(errors_path):

            for file in sorted(files):
                if file[-len('.asteriacore'):] == '.asteriacore':
                    files_to_test.append((dir_path, file))

    else:

        for file in files:
            if '/' in file:
                i = file.rindex('/')
                files_to_test.append((file[:i], file[i+1:]))
            else:
                files_to_test.append(('.', file))

    # Testing Errors

    print()
    print('ERROR TESTS ' + 68*'=')

    first = True
    for dir_path, file in files_to_test:

        test_path = Path(dir_path) / file

        if not os.path.exists(test_path):
            print(f'Invalid file path: {test_path}')

            continue

        with open(test_path) as f:
            src = f.read()

        cst = concrete.parse(test_path, src)

        mod = abstract.Module_from_cst(cst)

        elaborator = static.Elaborator()

        if show:
            print()

            if first:
                first = False
            else:
                print(80*'-')
                print()

            try:
                elaborator.elab_module(['Test'], mod)

            except errors.ElabError as err:
                print(err.pretty())

        else:

            with pytest.raises(errors.ElabError):

                elaborator.elab_module(['Test'], mod)

    print()

elif args[0] == 'elaboration':

    files_to_test = []

    files = args[1:]
    show = False

    if len(files) == 0:

        prefix_length = len(str(Path(__file__).parent))+1
        elab_path = Path(__file__).parent / 'elaboration'
        first = True
        for dir_path, dirs, files in os.walk(elab_path):

            for file in sorted(files):
                if file[-len('.asteriacore'):] == '.asteriacore':
                    files_to_test.append((dir_path, file))

    else:

        for file in files:
            if '/' in file:
                i = file.rindex('/')
                files_to_test.append((file[:i], file[i+1:]))
            else:
                files_to_test.append(('.', file))

    # Testing Errors

    print()
    print('ELABORATION TESTS ' + 68*'=')

    # prefix_length = len(str(Path(__file__).parent))+1
    # errors_path = Path(__file__).parent / 'errors'
    # first = True
    # for dir_path, dirs, files in os.walk(errors_path):
    #
    #     for file in sorted(files):
    #         if file[-len('.asteriacore'):] == '.asteriacore':

    no_errors = True
    first = True
    for dir_path, file in files_to_test:

        test_path = Path(dir_path) / file

        if not os.path.exists(test_path):
            print(f'Invalid file path: {test_path}')

            continue

        with open(test_path) as f:
            src = f.read()

        cst = concrete.parse(test_path, src)

        mod = abstract.Module_from_cst(cst)

        elaborator = static.Elaborator()

        try:
            elaborator.elab_module(['Test'], mod)

        except errors.ElabError as err:
            no_errors = False
            print()
            print(err.pretty())

    if no_errors:
        print()
        print('All elaboration tests successful.')

    print()
