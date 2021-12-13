def fib(n):
    cache = (0, 1)
    for _ in range(n):
        a, b = cache
        cache = (b, a + b)
    return cache[0]
