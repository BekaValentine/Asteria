import lark
from pathlib import Path

path = Path(__file__).parent / 'asteria.lark'
with path.open() as f:
    grammar = f.read()

parser = lark.Lark(grammar,
                   start='module',
                   parser='earley')


def parse(s):
    return parser.parse(s)
