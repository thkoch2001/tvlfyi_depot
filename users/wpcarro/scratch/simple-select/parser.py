class Parser(object):
    def __init__(self, tokens):
        self.tokens = tokens
        self.i = 0

    def exhausted(self):
        return self.i >= len(self.tokens)

    def peek(self, n=0):
        return self.tokens[self.i + n]

    def advance(self):
        if not self.exhausted():
            self.i += 1
        return self.peek(n=-1)

    def match(self, xs):
        if self.peek() in xs:
            self.advance()
            return True
        return False

    def test(self, predicate):
        return predicate(self.tokens, self.i)

    def expect(self, predicate):
        if self.exhausted():
            raise Exception("Unexpected EOL")
        if predicate(self.peek()):
            return self.advance()
        raise Exception("Unexpected token: \"{}\"".format(self.peek()))
