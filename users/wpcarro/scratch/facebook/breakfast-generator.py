# After being inspired by...
# craftinginterpreters.com/representing-code.html
# ...I'm implementing the breakfast generator that the author describes
# therein.

import random
import string

# Breakfast

def breakfast():
    fn = random.choice([
        lambda: " ".join([protein(), "with", breakfast(), "on the side"]),
        lambda: protein(),
        lambda: bread(),
    ])
    return fn()

def protein():
    fn = random.choice([
        lambda: " ".join([qualifier(), "crispy", "bacon"]),
        lambda: "sausage",
        lambda: " ".join([cooking_method(), "sausage"]),
    ])
    return fn()

def qualifier():
    fn = random.choice([
        lambda: "really",
        lambda: "super",
        lambda: " ".join(["really", qualifier()]),
    ])
    return fn()

def cooking_method():
    return random.choice([
        "scrambled",
        "poached",
        "fried",
    ])

def bread():
    return random.choice([
        "toast",
        "biscuits",
        "English muffin",
    ])

print(breakfast())

# Expression Language

# Because Python is a strictly evaluated language any functions that are
# mutually recursive won't terminate and will overflow our stack. Therefore, any
# non-terminals expressed in an alternative are wrapped in lambdas as thunks.

def expression():
    fn = random.choice([
        lambda: literal(),
        lambda: binary(),
    ])
    return fn()

def literal():
    return str(random.randint(0, 100))

def binary():
    return " ".join([expression(), operator(), expression()])

def operator():
    return random.choice(["+", "*"])

print(expression())

# Lox

def lox_expression():
    fn = random.choice([
        lambda: lox_literal(),
        lambda: lox_unary(),
        lambda: lox_binary(),
        lambda: lox_grouping(),
    ])
    return fn()

def lox_literal():
    fn = random.choice([
        lambda: str(random.randint(0, 100)),
        lambda: lox_string(),
        lambda: random.choice(["true", "false"]),
        lambda: "nil",
    ])
    return fn()

def lox_string():
    return "\"{}\"".format(
        "".join(random.choice(string.ascii_lowercase)
                for _ in range(random.randint(0, 25))))

def lox_grouping():
    return "(" + lox_expression() + ")"

def lox_unary():
    return random.choice(["-", "!"]) + lox_expression()

def lox_binary():
    return lox_expression() + lox_operator() + lox_expression()

def lox_operator():
    return random.choice(["==", "!=", "<", "<=", ">", ">=", "+", "-", "*", "/"])

print(lox_expression())
