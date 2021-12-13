def valid(take_out, dine_in, served):
    # edge case
    if len(take_out) + len(dine_in) != len(served):
        return False
    i = 0
    j = 0
    k = 0
    while i < len(take_out) and j < len(dine_in):
        if take_out[i] == served[k]:
            i += 1
        elif dine_in[j] == served[k]:
            j += 1
        else:
            return False
        k += 1
    # take out
    while i < len(take_out):
        if take_out[i] != served[k]:
            return False
        i += 1
    # dine in
    while j < len(dine_in):
        if dine_in[j] != served[k]:
            return False
        j += 1
    return True

take_out = [17, 8, 24]
dine_in  = [12, 19, 2]
served   = [17, 8, 12, 19, 24, 2]
result = valid(take_out, dine_in, served)
print(result)
assert result
print("Success!")
