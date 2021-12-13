from math import floor

def find_rotation(xs):
    if xs[0] < xs[-1]:
        return xs[0]
    beg, end = 0, len(xs) - 1
    found = False
    count = 10
    while not found and count >= 0:
        i = beg + floor((end - beg) / 2)
        if xs[beg] < xs[i]:
            beg = i
            i = beg + floor((end - beg) / 2)
        elif xs[beg] > xs[i]:
            end = i
        found = xs[i - 1] > xs[i]
        count -= 1
    return xs[i]


xs = [(['ptolemaic',
        'retrograde',
        'supplant',
        'undulate',
        'xenoepist',
        'zebra',
        'asymptote',
        'babka',
        'banoffee',
        'engender',
        'karpatka',
        'othellolagkage',
        ], "asymptote"),
      (['asymptote',
        'babka',
        'banoffee',
        'engender',
        'karpatka',
        'othellolagkage',
        ], "asymptote"),
      ]

for x, expected in xs:
    result = find_rotation(x)
    print(x, result)
    assert result == expected
    print("Success!")
