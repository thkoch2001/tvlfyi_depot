import time
import random
from collections import deque


class BoundedQueue(object):
    def __init__(self, size=0):
        """
        Returns a queue of elements that will never exceed `size`
        members. BoundedQueue evicts the oldest elements from itself before
        adding new elements.
        """
        self.xs = deque([None] * size)

    def add(self, x):
        """
        Add element `x` to the end of the queue. Evict the oldest element from
        the queue.
        """
        evicted = None
        if self.xs:
            evicted = self.xs.popleft()
            self.xs.append(x)
        return evicted


class Memo(object):
    def __init__(self, size=1):
        """
        Create a key-value data-structure that will never exceed `size`
        members. Memo evicts the oldest elements from itself before adding
        inserting new key-value pairs.
        """
        if size <= 0:
            raise Exception("We do not support an empty memo")
        self.xs = {}
        self.q = BoundedQueue(size=size)

    def contains(self, k):
        """
        Return true if key `k` exists in the Memo.
        """
        return k in self.xs

    def get(self, k):
        """
        Return the memoized item at key `k`.
        """
        return self.xs[k]

    def set(self, k, v):
        """
        Memoize value `v` at key `k`.
        """
        evicted = self.q.add(k)
        if evicted != None:
            del self.xs[evicted]
        self.xs[k] = v


memo = Memo(size=3)


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
