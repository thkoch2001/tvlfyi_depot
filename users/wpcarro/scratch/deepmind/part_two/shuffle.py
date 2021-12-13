import random


def get_random(floor, ceiling):
    return random.randrange(floor, ceiling + 1)


def shuffle(xs):
    n = len(xs)
    for i in range(n - 1):
        j = get_random(i + 1, n - 1)
        xs[i], xs[j] = xs[j], xs[i]


sample_list = [1, 2, 3, 4, 5]
print('Sample list:', sample_list)

print('Shuffling sample list...')
shuffle(sample_list)
print(sample_list)
