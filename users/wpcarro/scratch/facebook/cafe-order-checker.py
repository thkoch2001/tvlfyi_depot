def orders_are_sorted(take_out, dine_in, audit):
    if len(take_out) + len(dine_in) != len(audit):
        return False

    i, j = 0, 0
    for x in audit:
        if i < len(take_out) and take_out[i] == x:
            i += 1
        elif j < len(dine_in) and dine_in[j] == x:
            j += 1
        else:
            return False
    return True


assert orders_are_sorted([1,3,5], [2,4,6], [1,2,4,3,6,5])
assert not orders_are_sorted([1,3,5], [2,4,6], [1,2,4,5,6,3])
assert orders_are_sorted([], [2,4,6], [2,4,6])
print("Success!")
