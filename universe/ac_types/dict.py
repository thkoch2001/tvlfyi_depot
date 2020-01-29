# Naming options for this module:
# - map (like Elixir and standard.el): Overwrites python's built-in `map` function.
# - dict: Overwrites python's built-in `dict` function.


def take(ks, xs):
    result = {}
    for k in ks:
        result[k] = xs[k]
    return result
