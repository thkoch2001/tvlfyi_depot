# permutations: no repeat characters

def char_and_rest(i, xs):
    return xs[i], xs[0:i] + xs[i + 1:]

def permutations(xs):
    if len(xs) == 1:
        return [xs]
    result = []
    for c, rest in [char_and_rest(i, xs) for i in range(len(xs))]:
        result += [c + perm for perm in permutations(rest)]
    return result

expected = ["cat", "cta", "act", "atc", "tca", "tac"]
result = permutations("cat")
print(result, expected)
assert len(result) == len(expected)
assert result == expected
print("Success!")
