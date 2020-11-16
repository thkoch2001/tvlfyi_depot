def compute_hash(x):
    """
    Compute a unique fingerprint for the string input, `x`, as an integer using
    the following equation:

    x[0] * P^0 + x[1] * P^1 + ... x[n-1] * P^(n-1) % M

    P and M are constants.
    """
    p = 31 # small prime number
    m = int(10e9) + 9 # large prime number
    power = 0
    result = 0
    for c in x:
        result += ord(c) * p**power
        power += 1
    return result % m

class HashTable(object):
    def __init__(self, size):
        buckets = []
        for _ in range(size):
            buckets.append([])
        self.xs = buckets
        self.compute_hash = lambda k: compute_hash(k) % size

    def __repr__(self):
        result = []
        for bucket in self.xs:
            for entry in bucket:
                result.append(entry)
        return "HashTable({})".format(",".join(str(x) for x in result))

    def get(self, key):
        """
        Attempt to retrieve value stored under `key`.
        """
        h = self.compute_hash(key)
        for k, v in self.xs[h]:
            if k == key:
                return v
        return None

    def put(self, key, val):
        """
        Set `key` to `val`; update value at `key` if it already exists.
        """
        h = self.compute_hash(key)
        for i in range(len(self.xs[h])):
            # Update entry if the key exists...
            if self.xs[h][i][0] == key:
                self.xs[h][i] = (key, val)
                return None
        # ...create a new entry otherwise
        self.xs[h].append((key, val))

    def delete(self, key):
        h = self.compute_hash(key)
        self.xs[h] = list(filter(lambda entry: entry[0] != key, self.xs[h]))
