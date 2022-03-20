import lark
from indenter import Indenter


with open('asteria.lark', 'r') as f:
    grammar = f.read()


parser = lark.Lark(grammar,
                   start='module',
                   lexer='standard',
                   parser='earley',
                   postlex=Indenter())

if __name__ == '__main__':
    with open('test.asteria', 'r') as f:
        src = f.read()

    print(parser.parse(src.strip()).pretty())
