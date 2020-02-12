import unittest


################################################################################
# Solution
################################################################################
# bottom :: Rectangle -> Int
def bottom(x):
    return x.get('bottom_y')


# top :: Rectangle -> Int
def top(x):
    return bottom(x) + x.get('height')


# left :: Rectangle -> Int
def left(x):
    return x.get('left_x')


# right :: Rectangle -> Int
def right(x):
    return left(x) + x.get('width')


# sort_highest :: Rectangle -> Rectangle -> (Rectangle, Rectangle)
def sort_highest(x, y):
    if top(x) >= top(y):
        return x, y
    else:
        return y, x


# sort_leftmost :: Rectangle -> Rectangle -> (Rectangle, Rectangle)
def sort_leftmost(x, y):
    if left(x) <= left(y):
        return x, y
    else:
        return y, x


# rectify :: Int -> Int -> Int -> Int -> Rectify
def rectify(top=None, bottom=None, left=None, right=None):
    assert top >= bottom
    assert left <= right
    return {
        'left_x': left,
        'bottom_y': bottom,
        'width': right - left,
        'height': top - bottom,
    }


# empty_rect :: Rectangle
def empty_rect():
    return {
        'left_x': None,
        'bottom_y': None,
        'width': None,
        'height': None,
    }


# find_rectangular_overlap :: Rectangle -> Rectangle -> Maybe(Rectangle)
def find_rectangular_overlap(x, y):
    ha, hb = sort_highest(x, y)
    la, lb = sort_leftmost(x, y)

    if bottom(hb) <= top(hb) <= bottom(ha) <= top(ha):
        return empty_rect()

    if left(la) <= right(la) <= left(lb) <= right(lb):
        return empty_rect()

    # We should have an intersection here.
    verts = [bottom(ha), top(ha), bottom(hb), top(hb)]
    verts.sort()
    horzs = [left(la), right(la), left(lb), right(lb)]
    horzs.sort()
    return rectify(top=verts[2],
                   bottom=verts[1],
                   left=horzs[1],
                   right=horzs[2])


################################################################################
# Tests
################################################################################
class Test(unittest.TestCase):
    def test_overlap_along_both_axes(self):
        rect1 = {
            'left_x': 1,
            'bottom_y': 1,
            'width': 6,
            'height': 3,
        }
        rect2 = {
            'left_x': 5,
            'bottom_y': 2,
            'width': 3,
            'height': 6,
        }
        expected = {
            'left_x': 5,
            'bottom_y': 2,
            'width': 2,
            'height': 2,
        }
        actual = find_rectangular_overlap(rect1, rect2)
        self.assertEqual(actual, expected)

    def test_one_rectangle_inside_another(self):
        rect1 = {
            'left_x': 1,
            'bottom_y': 1,
            'width': 6,
            'height': 6,
        }
        rect2 = {
            'left_x': 3,
            'bottom_y': 3,
            'width': 2,
            'height': 2,
        }
        expected = {
            'left_x': 3,
            'bottom_y': 3,
            'width': 2,
            'height': 2,
        }
        actual = find_rectangular_overlap(rect1, rect2)
        self.assertEqual(actual, expected)

    def test_both_rectangles_the_same(self):
        rect1 = {
            'left_x': 2,
            'bottom_y': 2,
            'width': 4,
            'height': 4,
        }
        rect2 = {
            'left_x': 2,
            'bottom_y': 2,
            'width': 4,
            'height': 4,
        }
        expected = {
            'left_x': 2,
            'bottom_y': 2,
            'width': 4,
            'height': 4,
        }
        actual = find_rectangular_overlap(rect1, rect2)
        self.assertEqual(actual, expected)

    def test_touch_on_horizontal_edge(self):
        rect1 = {
            'left_x': 1,
            'bottom_y': 2,
            'width': 3,
            'height': 4,
        }
        rect2 = {
            'left_x': 2,
            'bottom_y': 6,
            'width': 2,
            'height': 2,
        }
        expected = {
            'left_x': None,
            'bottom_y': None,
            'width': None,
            'height': None,
        }
        actual = find_rectangular_overlap(rect1, rect2)
        self.assertEqual(actual, expected)

    def test_touch_on_vertical_edge(self):
        rect1 = {
            'left_x': 1,
            'bottom_y': 2,
            'width': 3,
            'height': 4,
        }
        rect2 = {
            'left_x': 4,
            'bottom_y': 3,
            'width': 2,
            'height': 2,
        }
        expected = {
            'left_x': None,
            'bottom_y': None,
            'width': None,
            'height': None,
        }
        actual = find_rectangular_overlap(rect1, rect2)
        self.assertEqual(actual, expected)

    def test_touch_at_a_corner(self):
        rect1 = {
            'left_x': 1,
            'bottom_y': 1,
            'width': 2,
            'height': 2,
        }
        rect2 = {
            'left_x': 3,
            'bottom_y': 3,
            'width': 2,
            'height': 2,
        }
        expected = {
            'left_x': None,
            'bottom_y': None,
            'width': None,
            'height': None,
        }
        actual = find_rectangular_overlap(rect1, rect2)
        self.assertEqual(actual, expected)

    def test_no_overlap(self):
        rect1 = {
            'left_x': 1,
            'bottom_y': 1,
            'width': 2,
            'height': 2,
        }
        rect2 = {
            'left_x': 4,
            'bottom_y': 6,
            'width': 3,
            'height': 6,
        }
        expected = {
            'left_x': None,
            'bottom_y': None,
            'width': None,
            'height': None,
        }
        actual = find_rectangular_overlap(rect1, rect2)
        self.assertEqual(actual, expected)


unittest.main(verbosity=2)
