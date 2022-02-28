# According to Crafting Interpreters, the only two primitives that a
# scanner/lexer needs are peek and advance; other functions (e.g. match) are
# nice-to-haves.
class Scanner(object):
  def __init__(self, chars):
    self.i = 0
    self.chars = chars

  def exhausted(self):
    return self.i >= len(self.chars)

  def peek(self, n=0):
    return self.chars[self.i + n] if self.i in range(0, len(self.chars)) else '\0'

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
