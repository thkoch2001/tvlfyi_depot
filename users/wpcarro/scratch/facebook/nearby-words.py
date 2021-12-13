def nearby_chars(c):
    keyboard = [
        "qwertyuiop",
        "asdfghjkl",
        "zxcvbnm",
    ]

    for row in keyboard:
        for i in range(len(row)):
            if row[i] == c:
                result = set()
                if i + 1 < len(row):
                    result.add(row[i + 1])
                if i - 1 >= 0:
                    result.add(row[i - 1])
                return result

def is_word(word):
    words = {
        "hello",
    }
    return word in words

def nearby_words(x):
    result = set()
    for i in range(len(x)):
        for c in nearby_chars(x[i]):
            candidate = x[0:i] + c + x[i+1:]
            if is_word(candidate):
                result.add(candidate)
    return result

print(nearby_words('gello'))
