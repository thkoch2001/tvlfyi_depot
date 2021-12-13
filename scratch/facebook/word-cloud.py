def normalize(x):
    noise = ".,;-"
    for y in noise:
        if x.endswith(y):
            return normalize(x[0:-1])
        if x.startswith(y):
            return normalize(x[1:])
    return x.lower()

def word_cloud(xs):
    result = dict()

    for x in xs.split(' '):
        k = normalize(x)
        if k in result:
            result[k] += 1
        else:
            result[k] = 1

    return result

result = word_cloud("This is just the beginning. The UK will lockdown again.")
assert result.get('this') == 1
assert result.get('is') == 1
assert result.get('just') == 1
assert result.get('the') == 2
assert result.get('beginning') == 1
assert result.get('uk') == 1
assert result.get('will') == 1
assert result.get('lockdown') == 1
assert result.get('again') == 1
print("Success!")
