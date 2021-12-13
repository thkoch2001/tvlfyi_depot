import string
from parser import Parser

mapping = {
    1: "family",
    2: "person",
    3: "firstName",
    4: "lastName",
    5: "state",
}

def parse_int(i, xs):
    result = ""
    while i < len(xs) and xs[i] in string.digits:
        result += xs[i]
        i += 1
    return i, int(result)

def parse_string(i, xs):
    result = ""
    while xs[i+1] not in string.digits:
        result += xs[i]
        i += 1
    return i, result

def tokenize(xs):
    result = []
    i = 0
    while i < len(xs):
        if xs[i] in string.digits:
            i, n = parse_int(i, xs)
            result.append(n)
        elif xs[i] in string.ascii_letters:
            i, x = parse_string(i, xs)
            result.append(x)
        elif xs[i] == " ":
            i += 1
            continue
    return result

def parse(xs):
    parser = Parser(tokenize(xs))
    return parse_element(parser)

# Element   -> Tag Attribute* End Element* End ;
# Tag       -> INTEGER ;
# Value     -> STRING End ;
# Attribute -> Tag Value ;
# End       -> 0 ;

def parse_element(parser):
    if type(parser.curr()) == str:
        return parser.consume()
    tag_id = parser.expect_predicate(lambda x: type(x) == int)
    tag = mapping[tag_id]
    attrs = parse_attrs(parser)
    parser.expect([0])
    children = []
    while not parser.exhausted() and parser.curr() != 0:
        children.append(parse_element(parser))
    parser.expect([0])
    return [tag, attrs, children]

def parse_attrs(parser):
    result = []
    while parser.curr() != 0:
        tag_id = parser.expect_predicate(lambda x: type(x) == int)
        tag = mapping[tag_id]
        value = parser.consume()
        result.append((tag, value))
    return result

def stringify_xml(tree, indent=0):
    if type(tree) == str:
        return tree
    result = ""
    tag, attrs, children = tree

    str_attrs = []
    for k, v in attrs:
        str_attrs.append("{}=\"{}\"".format(k, v))
    str_attrs = (" " if str_attrs else "") + " ".join(str_attrs)

    str_children = []
    for child in children:
        str_children.append(" " * 2 * indent + stringify_xml(child, indent + 1))
    str_children = "\n".join(str_children)

    result += "{}<{}{}>\n{}{}\n{}</{}>".format(
        " " * 2 * indent, tag, str_attrs, " " * 2 * indent, str_children,
        " " * 2 * indent, tag)
    return result

x = "1 4 McDowell 5 CA 0 2 3 Gayle 0 Some Message 0 0"
print("Input:   {}".format(x))
print("Tokens:  {}".format(tokenize(x)))
print("Parsed:  {}".format(parse(x)))
print("{}".format(stringify_xml(parse(x))))
