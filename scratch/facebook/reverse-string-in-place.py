# reverse :: [Char] -> ()
def reverse(xs):
    i = 0
    j = len(xs) - 1
    while i < j:
        xs[i], xs[j] = xs[j], xs[i]
        i += 1
        j -= 1

xs = [list("testing"), list("a"), list("to")]
for x in xs:
    print(x)
    reverse(x)
    print(x)
