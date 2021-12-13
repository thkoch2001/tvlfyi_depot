


# merge :: [a] -> [a] -> [a]
# merge([], []): []
# merge(xs, []): xs
# merge([], ys): ys
# merge(xs@[x|xs'], ys@[y|ys'])
#   when y =< x: cons(y, merge(xs, ys'))
#   when x < y:  cons(x, merge(xs', ys))
def merge(xs, ys):
    if xs == [] and ys == []:
        return []
    elif ys == []:
        return xs
    elif xs == []:
        return ys
    else:
        x = xs[0]
        y = ys[0]

        if y <= x:
            return [y] + merge(xs, ys[1:])
        else:
            return [x] + merge(xs[1:], ys)
        
print(merge([3, 4, 6, 10, 11, 15],
            [1, 5, 8, 12, 14, 19]))
