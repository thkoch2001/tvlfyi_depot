def substring_exists(corpus, pattern):
    """
    Return True if `pattern` appears in `corpus`.

    This function runs in O(m) time where n is equal to the length of
    `corpus`. To improve the efficiency of this algorithm, use a hashing
    function the reduces the number of collisions, which will consequently
    reduce the number of string-to-string, linear comparisons.
    """
    m, n = len(corpus), len(pattern)
    a = sum(ord(c) for c in corpus[0:n])
    b = sum(ord(c) for c in pattern)

    # (clumsily) prevent an off-by-one error...
    if a == b and corpus[0:n] == pattern:
        return True

    for i in range(1, m - n):
        # Update the hash of corpus by subtracting the hash of the character
        # that is sliding out of view and adding the hash of the character that
        # is sliding into view.
        a = a - ord(corpus[i - 1]) + ord(corpus[i + n - 1])
        # Integer comparison in O(0) time followed by string comparison in O(m)
        # time.
        if a == b and corpus[i:i + n] == pattern:
            return True
    return False
