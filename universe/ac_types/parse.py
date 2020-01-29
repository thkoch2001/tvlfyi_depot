import re
import string
from test_utils import simple_assert


def if_empty(x, parser):
    """If the field is empty, use `x`, otherwise, call `parser` on it."""
    def fn(y):
        if y == "":
            return x
        else:
            return parser(y)

    return fn


# nullable :: Parser -> Parser
def nullable(parser):
    def fn(x):
        if x == "":
            return None
        else:
            return parser(x)

    return fn


def required(column_name, parser):
    def fn(x):
        if x == "":
            raise Exception(
                "\"{}\" is a required field and cannot be empty".format(
                    column_name))
        else:
            return parser(x)

    return fn


def apply_parser(parser, row):
    """Calls each value in `parser` on the corresponding field in the
    dictionary, `row`."""
    result = {}
    for k, fn in parser.items():
        result[k] = fn(row[k])
    return result


def raise_parse_error(x, expected):
    """Raises a generic `Exception` when `x` is not a member of the `expected`
    set."""
    raise Exception("\"{}\" is none of the following: \"{}\"".format(
        x, ", ".join(expected)))


def as_hours(x):
    match = re.search(r'(\d+) hours', x)
    if match:
        try:
            return int(match[1])
        except:
            raise Exception('Failed to parse {} as an int'.format(match[1]))
    else:
        raise Exception('Failed to parse {} as hours'.format(x))


actual = as_hours('24 hours')
expected = 24
simple_assert(actual, expected, name='as_hours')


def as_mapping(mapping):
    def fn(x):
        if mapping[x]:
            return mapping[x]
        else:
            raise_parse_error(x, set(mapping.keys()))

    return fn


# as_yes_no :: String -> Boolean
def as_yes_no(x):
    """Attempt to parse `x`, a Yes or No value, into a boolean."""
    if x == "Yes":
        return True
    elif x == "No":
        return False
    else:
        raise_parse_error(x, {"Yes", "No"})


# as_data_source_type :: String -> String
def as_data_source_type(x):
    """Attempt to parse `x` as the Data Source Type column, which is an enum
    defined in the go/consult-types-authwf-auto-consult sheet."""
    acceptable = {"Hard-Code", "N/A", "Atlas", "Form"}
    if x not in acceptable:
        raise_parse_error(x, acceptable)
    return x


# as_type :: String -> String
def as_type(x):
    """Attempt to parse `x` as the Type column, which is an enum defined in the
    go/consult-types-authwf-auto-consult sheet."""
    acceptable = {
        "Parent Routing FRD", "Consult Parameter", "Consult Routing FRD",
        "Input", "Return Routing FRD"
    }
    if x not in acceptable:
        raise_parse_error(x, acceptable)
    return x


def as_int(x):
    return int(x)


def as_union_type(x):
    if " or " in x:
        return [string.trim_surrounding('"', x) for x in x.split(" or ")]
    else:
        return [x]


simple_assert(as_union_type('Non-Union'), ['Non-Union'], name='as_union_type')

simple_assert(as_union_type(
    '"Germany" or "Netherlands" or "Spain" or "Check Republic" or "United Kingdom"'
), ['Germany', 'Netherlands', 'Spain', 'Check Republic', 'United Kingdom'],
              name='as_union_type')


# identity :: a -> a
def identity(x):
    """Returns `x` unchanged."""
    return x
