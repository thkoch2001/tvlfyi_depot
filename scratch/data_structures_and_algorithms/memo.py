import time
import random
from heapq import heappush, heappop


class Memo(object):
    def __init__(self, size=1):
        """
        Create a key-value data-structure that will never exceed `size`
        members. Memo evicts the least-recently-accessed elements from itself
        before adding inserting new key-value pairs.
        """
        if size <= 0:
            raise Exception("We do not support an empty memo")
        self.xs = {}
        self.heap = [(0, None)] * size

    def contains(self, k):
        """
        Return true if key `k` exists in the Memo.
        """
        return k in self.xs

    def get(self, k):
        """
        Return the memoized item at key `k`.
        """
        # "touch" the element in the heap
        return self.xs[k]

    def set(self, k, v):
        """
        Memoize value `v` at key `k`.
        """
        _, to_evict = heappop(self.heap)
        if to_evict != None:
            del self.xs[to_evict]
        heappush(self.heap, (time.time(), k))
        self.xs[k] = v


memo = Memo(size=10)


def f(x):
    """
    Compute some mysterious, expensive function.
    """
    if memo.contains(x):
        print("Hit.\t\tf({})".format(x))
        return memo.get(x)
    else:
        print("Computing...\tf({})".format(x))
        time.sleep(0.25)
        res = random.randint(0, 10)
        memo.set(x, res)
        return res


[f(random.randint(0, 10)) for _ in range(10)]
