import random
from collections import deque

def sorted(xs):
    result = [0] * 100
    for x in xs:
        result[x - 1] += 1

    answer = deque()
    for i in range(len(result)):
        x = result[i]
        for _ in range(x):
            answer.appendleft(i + 1)

    return list(answer)

scores = [random.choice(range(70, 100)) for _ in range(20)]
print(scores)
result = sorted(scores)
print(result)
