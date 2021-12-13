from parser import Parser

# As an exercise to stress-test my understanding of recursive descent parsers,
# I'm attempting to write a JSON parser without referencing any existing BNF
# descriptions of JSON or existing JSON parser implementations.
#
# I'm only parsing a subset of JSON: enough to parse `sample`. Here is the BNF
# that I wrote to describe my expected input:
#
# expression -> object
# object     -> '{' ( STRING ':' expression ) ( ',' STRING ':' expression )* '}'
#            |  array
# array      -> '[' expression ( ',' expression )* ']'
#            |  literal
# literal    -> STRING | INT

def tokenize(xs):
    """
    Return a list of tokens from the string input, `xs`.
    """
    result = []
    i = 0
    while i < len(xs):
        # single characters
        if xs[i] in ",{}:[]":
            result.append(xs[i])
            i += 1
        # strings
        elif xs[i] == "\"":
            curr = xs[i]
            i += 1
            while xs[i] != "\"":
                curr += xs[i]
                i += 1
            curr += xs[i]
            result.append(curr)
            i += 1
        # integers
        elif xs[i] in "0123456789":
            curr = xs[i]
            i += 1
            while xs[i] in "0123456789":
                curr += xs[i]
                i += 1
            result.append(int(curr))
        # whitespace
        elif xs[i] in {" ", "\n"}:
            i += 1
    return result

def parse_json(x):
    """
    Attempt to parse the string, `x`, into JSON.
    """
    tokens = tokenize(x)
    return parse_object(Parser(tokens))

def parse_object(parser):
    if parser.match(['{']):
        key = parse_string(parser)
        parser.expect([':'])
        value = parse_object(parser)
        result = [(key, value)]
        while parser.match([',']):
            key = parse_string(parser)
            parser.match([':'])
            value = parse_object(parser)
            result.append((key, value))
        return result
    return parse_array(parser)

def parse_array(parser):
    if parser.match(['[']):
        if parser.match([']']):
            return []
        result = [parse_object(parser)]
        while parser.match([',']):
            result.append(parse_object(parser))
        parser.expect([']'])
        return result
    else:
        return parse_literal(parser)

def parse_string(parser):
    if parser.curr().startswith("\""):
        return parser.consume()
    else:
        raise Exception("Unexpected token: {}".format(parser.curr()))

def parse_literal(parser):
    return parser.consume()

sample = """
{
  "glossary": {
    "title": "example glossary",
    "GlossDiv": {
      "title": "S",
      "GlossList": {
        "GlossEntry": {
          "ID": "SGML",
          "SortAs": "SGML",
          "GlossTerm": "Standard Generalized Markup Language",
          "Acronym": "SGML",
          "Abbrev": "ISO 8879:1986",
          "GlossDef": {
            "para": "A meta-markup language, used to create markup languages such as DocBook.",
            "GlossSeeAlso": [
              "GML",
              "XML"
            ]
          },
          "GlossSee": "markup"
        }
      }
    }
  }
}
"""

print(parse_json(sample))
