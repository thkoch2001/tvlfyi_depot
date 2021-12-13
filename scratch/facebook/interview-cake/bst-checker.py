def is_valid(node):
    """
    Return True if `node` is a valid binary search tree.
    """
    s = []
    s.append((float('-inf'), node, float('inf')))
    while s:
        lo, node, hi = s.pop()
        if lo <= node.value <= hi:
            node.lhs and s.append((lo, node.lhs, node.value))
            node.rhs and s.append((node.value, node.rhs, hi))
        else:
            return False
    return True
