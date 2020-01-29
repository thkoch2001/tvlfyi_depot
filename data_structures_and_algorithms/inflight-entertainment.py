# possible :: Int -> [Int] -> Bool
def possible(flight_duration, film_durations):
    seeking = set()

    for x in film_durations:
        if x in seeking:
            return True
        else:
            seeking.add(flight_duration - x)

    return False


should = [
    (10, [1, 9, 8, 8, 8]),
    (10, [1, 9]),
    (10, [1, 9, 5, 5, 6]),
    (1, [0.5, 0.5]),
    (1, [0.5, 0.5]),
]

for a, b in should:
    print("Testing: %s %s" % (a, b))
    assert possible(a, b)

shouldnt = [
    (10, [1, 10, 1, 2, 1, 12]),
    (1, [0.25, 0.25, 0.25, 0.25]),
    (5, [1, 2, 2]),
]
for a, b in shouldnt:
    print("Testing: %s %s" % (a, b))
    assert not possible(a, b)

print("Tests pass")
