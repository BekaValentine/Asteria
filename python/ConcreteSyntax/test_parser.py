import lark
from lark.indenter import Indenter


with open('asteria.lark', 'r') as f:
    grammar = f.read()


class TreeIndenter(Indenter):
    NL_type = '_NEWLINE'
    OPEN_PAREN_types = ['_LPAR']
    CLOSE_PAREN_types = ['_RPAR']
    INDENT_type = '_INDENT'
    DEDENT_type = '_DEDENT'
    tab_len = 2


parser = lark.Lark(grammar,
                   start='module',
                   lexer='standard',
                   parser='earley',
                   postlex=TreeIndenter())

if __name__ == '__main__':
    with open('test.asteria', 'r') as f:
        src = f.read()

    print(parser.parse(src).pretty())
