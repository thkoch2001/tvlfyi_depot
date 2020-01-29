from test_utils import simple_assert


def pipe(x, fns):
    """Apply `x` to the first function in `fns` and then use its output as the
    input for the next function in the list."""
    result = x
    for f in fns:
        result = f(result)
    return result


actual = pipe(10, [
    lambda x: x + 3,
    lambda x: x - 1,
    lambda x: x * 2,
])
expected = (((10 + 3) - 1) * 2)
simple_assert(actual, expected, name="pipe")
