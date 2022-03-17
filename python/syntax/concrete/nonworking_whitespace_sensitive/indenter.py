"Provides Indentation services for languages with indentation similar to Haskell"

import lark
from lark import LarkError
from lark.lexer import Token


class DedentError(LarkError):
    pass


class Indent(object):
    def __init__(self, indent_type, depth):
        self.indent_type = indent_type
        self.depth = depth

    def __repr__(self):
        return '%s(%i)' % (self.indent_type, self.depth)


class Indenter():
    NL_type = '_NEWLINE'
    OPEN_PAREN_types = ['_LPAR']
    CLOSE_PAREN_types = ['_RPAR']
    INDENT_triggers = ['_IMPORTING', '_WHERE', '_OF', '_LET']
    INDENT_type = '_INDENT'
    DEDENT_type = '_DEDENT'
    tab_len = 2
    always_accept = ()

    def __init__(self):
        self.indents = []
        self.last_seen_was_indent_trigger = False

    def handle_NL(self, token):
        indent_str = token.rsplit('\n', 1)[1]  # Tabs and spaces
        indent = indent_str.count(' ') + indent_str.count('\t') * self.tab_len

        if len(self.indents) == 0:
            if self.last_seen_was_indent_trigger:
                # we're adding a new block indent depth
                self.indents.append(Indent('block', indent))
                yield Token.new_borrow_pos('_INDENT', indent_str, token)
            else:
                # we're adding a new typographical indent depth
                self.indents.append(Indent('typographical', indent))
                yield Token.new_borrow_pos('WS_INLINE', indent_str, token)

        else:
            if indent > self.indents[-1].depth:
                # this indents further

                if self.last_seen_was_indent_trigger:
                    # we're adding a new block indent depth
                    self.indents.append(Indent('block', indent))
                    yield Token.new_borrow_pos('_INDENT', indent_str, token)
                else:
                    # we're adding a new typographical indent depth
                    self.indents.append(Indent('typographical', indent))
                    yield Token.new_borrow_pos('WS_INLINE', indent_str, token)
            elif indent == self.indents[-1].depth:
                # this is a samedent
                if self.indents[-1].indent_type == 'typographic':
                    yield Token.new_borrow_pos('WS_INLINE', indent_str, token)
                elif self.indents[-1].indent_type == 'block':
                    yield Token.new_borrow_pos('_SAMEDENT', indent_str, token)
            else:
                # this is a dedent possibly, so we peal off the indents that are
                # to the right of this one, remembering the number of the block indents
                print('Dedent!')
                print(self.indents)
                print(indent)
                while len(self.indents) > 0 and self.indents[-1].depth > indent:
                    if self.indents[-1].indent_type == 'block':
                        print('yielding dedent')
                        yield Token.new_borrow_pos(self.DEDENT_type, indent_str, token)
                    self.indents.pop()

                print(self.indents)
                print(indent)

                if len(self.indents) > 0:
                    if self.indents[-1].depth == indent:
                        if self.indents[-1].indent_type == 'block':
                            yield Token.new_borrow_pos('_SAMEDENT', indent_str, token)
                        elif self.last_seen_was_indent_trigger:
                            # we're adding a new block indent depth
                            self.indents.append(Indent('block', indent))
                            yield Token.new_borrow_pos('_INDENT', indent_str, token)
                        else:
                            # we're adding a new typographical indent depth
                            self.indents.append(
                                Indent('typographical', indent))
                            yield Token.new_borrow_pos('WS_INLINE', indent_str, token)
                    elif self.indents[-1].depth < indent:
                        # if we're still indented past the top, we need to have new indents
                        if self.last_seen_was_indent_trigger:
                            # we're adding a new block indent depth
                            self.indents.append(Indent('block', indent))
                            yield Token.new_borrow_pos('_INDENT', indent_str, token)
                        else:
                            # we're adding a new typographical indent depth
                            self.indents.append(
                                Indent('typographical', indent))
                            yield Token.new_borrow_pos('WS_INLINE', indent_str, token)
                # else:
                #     if self.last_seen_was_indent_trigger:
                #         # we're adding a new block indent depth
                #         self.indents.append(Indent('block', indent))
                #         yield Token.new_borrow_pos('_INDENT', indent_str, token)
                #     else:
                #         # we're adding a new typographical indent depth
                #         self.indents.append(
                #             Indent('typographical', indent))
                #         yield Token.new_borrow_pos('WS_INLINE', indent_str, token)

                    # if indent > self.indent_level[-1]:
                    #     if self.last_seen_was_ident_trigger:
                    #         self.indent_level.append(indent)
                    #         yield Token.new_borrow_pos(self.INDENT_type, indent_str, token)
                    #     else:
                    #         yield token
                    # else:
                    #     while indent < self.indent_level[-1]:
                    #         self.indent_level.pop()
                    #         yield Token.new_borrow_pos(self.DEDENT_type, indent_str, token)
                    #
                    #     if indent != self.indent_level[-1]:
                    #         raise DedentError('Unexpected dedent to column %s. Expected dedent to %s' % (
                    #             indent, self.indent_level[-1]))
                    #
                    #     yield token

    def _process(self, stream):
        for token in stream:
            print('TOKEN: ', repr(token))
            if token.type == self.NL_type:
                for t in self.handle_NL(token):
                    print('EMITTED: ', repr(t))
                    yield t
            else:
                self.last_seen_was_indent_trigger = token.type in self.INDENT_triggers
                print('EMITTED: ', repr(token))
                yield token

        print('processed tokens, cleaning up')
        print(self.indents)
        while len(self.indents) > 0:
            if self.indents[-1].indent_type == 'block' and self.indents[-1].depth > 0:
                print('EMITTED: ', repr(Token(self.DEDENT_type, '')))
                yield Token(self.DEDENT_type, '')
            self.indents.pop()

    def process(self, stream):
        return stream
        return self._process(stream)

# }
