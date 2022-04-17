import lark
from pathlib import Path

path = Path(__file__).parent / 'concrete.lark'
with path.open() as f:
    grammar = f.read()


def parse(file_path, s):
    lines = s.split('\n')

    parser = lark.Lark(grammar,
                       start='module',
                       parser='earley',
                       propagate_positions=True)

    class SourceAnnotator(lark.visitors.Visitor_Recursive):
        def __default__(self, tree):
            tree.source_info = {
                'path': str(file_path),
                'text_lines': lines
            }

    t = parser.parse(s)
    SourceAnnotator().visit(t)
    return t
