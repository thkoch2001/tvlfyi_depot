import unittest


# Solution
def is_first_come_first_served(xs, ys, zs):
    i, j = 0, 0
    for z in zs:
        if i < len(xs) and z == xs[i]:
            i += 1
        elif j < len(ys) and z == ys[j]:
            j += 1
        else:
            return False
    return i == len(xs) and j == len(ys)


# Tests
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

    # Bonus
    def test_handles_repeats(self):
        actual = is_first_come_first_served([1, 2, 1], [3, 4, 5, 5],
                                            [3, 4, 1, 5, 5, 2, 1])
        self.assertTrue(actual)

    def test_kitchen_didnt_serve(self):
        actual = is_first_come_first_served([1, 2], [3, 4], [1, 3, 4])
        self.assertFalse(actual)

    def test_customer_didnt_pay(self):
        actual = is_first_come_first_served([2], [3, 4], [1, 3, 4])
        self.assertFalse(actual)


unittest.main(verbosity=2)
