from argparse import ArgumentParser

import csv
import sqlite3
import string
from scanner import Scanner
from parser import Parser
import re

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

AND    = ("CONJUNCTION", "AND")
OR     = ("CONJUNCTION", "OR")
NOT    = ("PUNCTUATION", "NOT")
COLON  = ("PUNCTUATION", "COLON")
LPAREN = ("PUNCTUATION", "LPAREN")
RPAREN = ("PUNCTUATION", "RPAREN")

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
      "-": NOT,
      ":": COLON,
      "(": LPAREN,
      ")": RPAREN,
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
  conjunctions = {
      "AND",
      "OR",
  }
  current = s.advance()
  while is_alphanumeric(s.peek()):
    current += s.advance()
  if current.upper() in conjunctions:
    return ("CONJUNCTION", current.upper())
  else:
    return ("IDENTIFIER", current)

################################################################################
# Parser
################################################################################

# EBNF
# Note: we order expression types by ascending levels of precedence.
#
# expression  -> conjunction ;
# conjunction -> selection ( ( "AND" | "OR" )? selection )* ;
# selection   -> "-"? IDENTIFIER ":" ( REGEX | STRING ) | grouping ;
# grouping    -> REGEX | STRING | "(" expression ")" ;

def parse(x):
  tokens = tokenize(x)
  p = Parser(tokens)
  return expression(p)

def expression(p):
  return conjunction(p)

def conjunction(p):
  lhs = selection(p)

  # TODO(wpcarro): Support default AND conjuctions when they're undefined.
  while not p.exhausted() and p.match({AND, OR}):
    conj = p.peek(n=-1)
    rhs = selection(p)
    lhs = ("CONJUNCTION", conj[1], lhs, rhs)

  return lhs

def selection(p):
  negate = False
  if p.peek() == NOT:
    negate = True
    p.advance()

  if p.peek()[0] != "IDENTIFIER":
    return grouping(p)

  ident = p.expect(lambda x: x[0] == "IDENTIFIER")
  colon = p.expect(lambda x: x[1] == "COLON")
  value = p.expect(lambda x: x[0] in {"REGEX", "STRING"})
  return ("SELECTION", negate, ident[1], value)

def grouping(p):
  if p.peek()[0] == "REGEX":
    return p.advance()

  if p.peek()[0] == "STRING":
    return p.advance()

  if p.peek() == LPAREN:
    p.advance()
    expr = expression(p)
    p.expect(lambda x: x == RPAREN)
    return ("GROUPING", expr)

################################################################################
# Compiler
################################################################################

def compile(source, table, columns):
  ast = parse(source)
  return "SELECT * FROM {} WHERE {};".format(table, do_compile(ast, columns))

def do_compile(ast, columns):
  if ast[0] == "REGEX":
    cols = "({})".format(" || ".join(columns))
    return "{} REGEXP '.*{}.*'".format(cols, ast[1])

  if ast[0] == "STRING":
    cols = "({})".format(" || ".join(columns))
    return "{} LIKE '%{}%'".format(cols, ast[1])

  if ast[0] == "SELECTION":
    return compile_selection(ast)

  if ast[0] == "CONJUNCTION":
    _, conj, lhs, rhs = ast
    lhs = do_compile(lhs, columns)
    rhs = do_compile(rhs, columns)
    return "{} {} {}".format(lhs, conj, rhs)

  if ast[0] == "GROUPING":
    return "({})".format(do_compile(ast[1], columns))

  raise Exception("Unexpected AST: \"{}\"".format(ast))

def compile_selection(ast):
  _, negate, column, query = ast
  match = compile_query(negate, query)
  return "{} {}".format(column, match)

def compile_query(negate, query):
  query_type, query_string = query
  if query_type == "REGEX":
    if negate:
      return "NOT REGEXP '.*{}.*'".format(query_string)
    return "REGEXP '.*{}.*'".format(query_string)

  if query_type == "STRING":
    if negate:
      return "NOT LIKE '%{}%'".format(query_string)
    return "LIKE '%{}%'".format(query_string)

################################################################################
# Helper Functions
################################################################################

def regexp(expr, x):
  reg = re.compile(expr)
  return reg.search(x) is not None

################################################################################
# Main
################################################################################

def main(csv_path=None, debug=False):
  # Import CSV to SQLite
  table = "main"
  con = sqlite3.connect(":memory:")

  con.create_function("REGEXP", 2, regexp)

  cur = con.cursor()
  with open(csv_path, "r") as f:
    r = csv.DictReader(f)
    columns = next(r).keys()

    # TODO(wpcarro): Use safer interpolation variant of "?" here and throughout.
    cur.execute("CREATE TABLE {} ({});".format(table, ",".join(columns)))
    rows = [tuple(row[col] for col in columns) for row in r]
    cur.executemany("INSERT INTO {} ({}) VALUES ({});".format(table, ",".join(columns), ",".join("?" for _ in columns)), rows)
    con.commit()

  while True:
    x = input("> ")

    if debug:
      print("tokens:\t{}".format(tokenize(x)))
      print("AST:\t{}".format(parse(x)))
      print("query:\t\"{}\"".format(compile(x, table, columns)))

    for row in cur.execute(compile(x, table, columns)):
      print("\t".join(str(cell) for cell in row))

  # TODO(wpcarro): Trap exits and ensure cleanup always runs.
  con.close()

if __name__ == "__main__":
  parser = ArgumentParser()
  parser.add_argument("-f", "--file", dest="file", help="Path to the CSV from which to read", metavar="PATH")
  parser.add_argument("-d", "--debug", dest="debug", default=False, action="store_true", help="Enable debugging")
  args = parser.parse_args()
  main(csv_path=args.file, debug=args.debug)
