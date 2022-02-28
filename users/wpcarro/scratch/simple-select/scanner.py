# According to Crafting Interpreters, the only two primitives that a
# scanner/lexer needs are peek and advance; other functions (e.g. match) are
# nice-to-haves.
class Scanner(object):
  def __init__(self, source):
    self.i = 0
    self.source = source

  def exhausted(self):
    return self.i >= len(self.source)

  def peek(self, n=0):
    return self.source[self.i + n] if self.i + n < len(self.source) else '\0'

  def advance(self):
    result = self.peek()
    self.i += 1
    return result

  def match(self, x):
    if self.exhausted():
      return False
    if self.peek() == x:
      self.advance()
      return True
    else:
      return False
