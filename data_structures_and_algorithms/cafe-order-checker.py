import unittest


################################################################################
# Implementation
################################################################################
def is_first_come_first_served(to, di, xs):
    # All the guards, assertions we should need.
    if to == di == xs == []:
        return True
    elif to == di == []:
        return False
    elif to == []:
        return di == xs
    elif to == []:
        return di == xs
    elif di == []:
        return to == xs
    elif xs == []:
        return False
    elif len(xs) != (len(to) + len(di)):
        return False

    fst, snd = to, di

    if xs[0] == to[0]:
        fst, snd = to, di
    elif xs[0] == di[0]:
        fst, snd = di, to
    else:
        return False

    fst_done, snd_done = False, False
    fi, si = 1, 0

    for i in range(1, len(xs)):
        # Short-circuit and avoid index-out-of-bounds without introducing overly
        # defensive, sloppy code.
        if fst_done:
            return snd[si:] == xs[i:]
        elif snd_done:
            return fst[fi:] == xs[i:]

        if fst[fi] == xs[i]:
            fi += 1
        elif snd[si] == xs[i]:
            si += 1
        else:
            return False

        fst_done, snd_done = fi == len(fst), si == len(snd)

    return True


################################################################################
# Tests
################################################################################
class Test(unittest.TestCase):
    def test_both_registers_have_same_number_of_orders(self):
        result = is_first_come_first_served([1, 4, 5], [2, 3, 6],
                                            [1, 2, 3, 4, 5, 6])
        self.assertTrue(result)

    def test_registers_have_different_lengths(self):
        result = is_first_come_first_served([1, 5], [2, 3, 6], [1, 2, 6, 3, 5])
        self.assertFalse(result)

    def test_one_register_is_empty(self):
        result = is_first_come_first_served([], [2, 3, 6], [2, 3, 6])
        self.assertTrue(result)

    def test_served_orders_is_missing_orders(self):
        result = is_first_come_first_served([1, 5], [2, 3, 6], [1, 6, 3, 5])
        self.assertFalse(result)

    def test_served_orders_has_extra_orders(self):
        result = is_first_come_first_served([1, 5], [2, 3, 6],
                                            [1, 2, 3, 5, 6, 8])
        self.assertFalse(result)

    def test_one_register_has_extra_orders(self):
        result = is_first_come_first_served([1, 9], [7, 8], [1, 7, 8])
        self.assertFalse(result)

    def test_one_register_has_unserved_orders(self):
        result = is_first_come_first_served([55, 9], [7, 8], [1, 7, 8, 9])
        self.assertFalse(result)


unittest.main(verbosity=2)
