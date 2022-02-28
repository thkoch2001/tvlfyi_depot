import string
from scanner import Scanner
################################################################################
# Predicates
################################################################################

def is_alpha(c):
  return c in string.ascii_letters

def is_digit(c):
  return c in "0123456789"

def is_alphanumeric(c):
  return is_alpha(c) or is_digit(c)

def is_whitespace(c):
  return c in " \r\t\n"

################################################################################
# Tokenizer
################################################################################

def tokenize(x):
  s = Scanner(x)
  tokens = scan_tokens(s)
  return tokens

def scan_tokens(s):
  result = []
  while not s.exhausted():
    if is_whitespace(s.peek()):
      s.advance()
    else:
      result.append(scan_token(s))
  return result

def scan_token(s):
  punctuation = {
      "-": "NOT",
      ":": "COLON",
  }
  c = s.peek()
  if c in punctuation:
    s.advance()
    return punctuation[c]
  if c == "\"":
    return tokenize_string(s)
  if c == "/":
    return tokenize_regex(s)
  if is_alpha(c):
    return tokenize_identifier(s)

def tokenize_string(s):
  s.advance() # ignore opening 2x-quote
  current = ""
  while s.peek() != "\"" and not s.exhausted():
    current += s.advance()
  if s.exhausted():
    raise Exception("Unterminated string")
  s.advance() # ignore closing 2x-quote
  return ("STRING", current)

def tokenize_regex(s):
  s.advance() # ignore opening forward-slash
  current = ""
  while s.peek() != "/" and not s.exhausted():
    current += s.advance()
  if s.exhausted():
    raise Exception("Unterminated regex")
  s.advance() # ignore closing forward-slash
  return ("REGEX", current)

def tokenize_identifier(s):
  keywords = {
      "AND",
      "OR",
  }
  current = s.advance()
  while is_alphanumeric(s.peek()):
    current += s.advance()
  if current.upper() in keywords:
    return ("KEYWORD", current.upper())
  else:
    return ("IDENTIFIER", current)

################################################################################
# Main
################################################################################

def main():
  while True:
    x = input("> ")
    print(tokenize(x))

if __name__ == "__main__":
  main()
