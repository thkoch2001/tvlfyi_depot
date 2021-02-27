import sys

def parse(line):
    a = line.split(sep=" ", maxsplit=1)
    assert len(a) == 2
    fromto = a[0].split(sep="-")
    assert len(fromto) == 2
    (from_, to) = (int(fromto[0]), int(fromto[1]))
    charpass = a[1].split(sep=": ")
    assert len(charpass) == 2
    char = charpass[0]
    assert len(char) == 1
    pass_ = charpass[1]
    assert pass_.endswith("\n")
    pass_ = pass_[:-1]
    return {
        "from": from_,
        "to": to,
        "char": char,
        "pass": pass_
    }

def char_in_pass(char, pass_):
    return pass_.count(char)

def validate_01(entry):
    no = char_in_pass(entry["char"], entry["pass"])
    if no < entry["from"]:
        return { "too-small": entry }
    elif no > entry["to"]:
        return { "too-big": entry }
    else:
        return { "ok": entry }

def char_at_pos(char, pos, pass_):
    assert pos <= len(pass_)
    return pass_[pos-1] == char

def validate_02(entry):
    one = char_at_pos(entry["char"], entry["from"], entry["pass"])
    two = char_at_pos(entry["char"], entry["to"], entry["pass"])
    if one and two:
        return { "both": entry }
    elif one:
        return { "one": entry }
    elif two:
        return { "two": entry }
    else:
        return { "none": entry }


res01 = []
res02 = []
with open("./input", 'r') as f:
    for line in f:
        p = parse(line)
        res01.append(validate_01(p))
        res02.append(validate_02(p))

count01=0
for r in res01:
    print(r)
    if r.get("ok", False):
        count01=count01+1

count02=0
for r in res02:
    print(r)
    if r.get("one", False):
        count02=count02+1
    elif r.get("two", False):
        count02=count02+1
    else:
        pass

print("count 1: {}".format(count01))
print("count 2: {}".format(count02))
