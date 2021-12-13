class Parser(object):
    def __init__(self, tokens):
        self.tokens = tokens
        self.i = 0

    def prev(self):
        return self.tokens[self.i - 1]

    def curr(self):
        return self.tokens[self.i]

    def consume(self):
        if not self.exhausted():
            self.i += 1
            return self.prev()

    def match(self, xs):
        if not self.exhausted() and self.curr() in xs:
            self.consume()
            return True
        return False

    def expect(self, xs):
        if not self.match(xs):
            raise Exception("Expected token \"{}\" but received \"{}\"".format(xs, self.curr()))

    def exhausted(self):
        return self.i >= len(self.tokens)
