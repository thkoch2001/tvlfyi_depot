
def merge(xs):
    xs.sort()
    result = xs[0:1]
    for a, b in xs[1:]:
        y, z = result[-1]
        if a <= z:
            result[-1] = (y, max(b, z))
        else:
            result.append((a, b))
    return result

inputs = [([(0,1),(3,5),(4,8),(10,12),(9,10)], [(0,1),(3,8),(9,12)]),
          ([(1,2),(2,3)], [(1,3)]),
          ([(1,5),(2,3)], [(1,5)]),
          ([(1,10),(2,6),(3,5),(7,9)], [(1,10)]),
          ]
for x, expected in inputs:
    result = merge(x)
    print(x)
    print(result)
    assert result == expected
    print("Success!")
