import lark
from lark.indenter import Indenter


with open('asteria2.lark', 'r') as f:
    grammar = f.read()


class TreeIndenter(Indenter):
    NL_type = '_NEWLINE'
    OPEN_PAREN_types = []
    CLOSE_PAREN_types = []
    INDENT_type = '_INDENT'
    DEDENT_type = '_DEDENT'
    tab_len = 2


parser = lark.Lark(grammar,
                   start='module',
                   lexer='standard',
                   parser='earley',
                   postlex=TreeIndenter())

if __name__ == '__main__':
    with open('test2.asteria', 'r') as f:
        src = f.read()

    print(parser.parse(src).pretty())
