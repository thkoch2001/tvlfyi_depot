from test_utils import simple_assert


def with_banner(x):
    header = '#################################################################'
    text = '# {}'.format(x)
    footer = '#################################################################'
    return '\n'.join([header, text, footer])


def starts_with(prefix, x):
    """Return True if `x` starts with `prefix`."""
    return x.startswith(prefix)


def ends_with(prefix, x):
    """Return True if `x` starts with `prefix`."""
    return x.endswith(prefix)


def trim_prefix(prefix, x):
    """Remove `prefix` from `x` if present."""
    if x.startswith(prefix):
        return x[len(prefix):]
    else:
        return x


actual = [trim_prefix('"', '"leading"'), trim_prefix('"', 'non-leading')]
expected = ['leading"', 'non-leading']
simple_assert(actual, expected, name="trim_prefix")


def trim_suffix(suffix, x):
    """Remove `suffix` from `x` if present."""
    if x.endswith(suffix):
        amt = len(x) - len(suffix)
        return x[0:amt]
    else:
        return x


actual = [
    trim_suffix('ing"', '"trailing"'),
    trim_suffix('ing"', 'non-trailing')
]
expected = ['"trail', 'non-trailing']
simple_assert(actual, expected, name="trim_suffix")


def trim_surrounding(b, x):
    """Remove `b` from `x` if present as prefix and suffix."""
    if x.startswith(b) and x.endswith(b):
        x = trim_prefix(b, x)
        x = trim_suffix(b, x)
        return x
    else:
        return x


actual = [
    trim_surrounding('"', '"surrounded"'),
    trim_surrounding('"', 'non-surrounded'),
    trim_surrounding('"', '"just-prefixed'),
    trim_surrounding('"', 'just-suffixed"'),
]
expected = ['surrounded', 'non-surrounded', '"just-prefixed', 'just-suffixed"']
simple_assert(actual, expected, name="trim_surrounding")


def indent(x, spaces=2):
    """Indent string `x` number of `spaces`, defaulting to two."""
    return '\n'.join([' ' * spaces + line for line in x.split('\n')])


actual = indent("""testing
this function
out""")
expected = """  testing
  this function
  out"""
simple_assert(actual, expected, name="indent")

actual = indent("""testing
this function
out""", spaces=4)
expected = """    testing
    this function
    out"""
simple_assert(actual, expected, name="indent")
