from random import choice
from utils import init_table

def get(movie, seeking):
    return any([movie in xs for xs in seeking.values()])

def set_complement(movie, seeking):
    for duration, xs in seeking.items():
        seeking[duration].add(duration - movie)

def choose_movies(tolerance, duration, movies):
    seeking = {duration + i: set() for i in range(-1 * tolerance, tolerance + 1)}
    for movie in movies:
        if get(movie, seeking):
            return movie, duration - movie
        else:
            set_complement(movie, seeking)
    return None

tolerance = 20
duration = choice([1, 2, 3]) * choice([1, 2]) * choice([15, 30, 45])
movies = [choice([1, 2, 3]) * choice([15, 30, 45]) for _ in range(10)]
print("Seeking two movies for a duration of [{}, {}] minutes".format(duration - tolerance, duration + tolerance))
print(movies)
result = choose_movies(tolerance, duration, movies)
if result:
    print("{} + {} = {}".format(result[0], result[1], duration))
else:
    print(":( We're sad because we couldn't find two movies for a {} minute flight".format(duration))
