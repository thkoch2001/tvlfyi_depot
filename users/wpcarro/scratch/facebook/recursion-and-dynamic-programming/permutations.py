def char_and_rest(i, xs):
    return xs[i], xs[:i] + xs[i+1:]

# perms :: String -> [String]
def perms(xs):
    if len(xs) == 1:
        return [xs]
    result = []
    for c, rest in [char_and_rest(i, xs) for i in range(len(xs))]:
        result += [c + perm for perm in perms(rest)]
    return result

print(perms("cat"))
