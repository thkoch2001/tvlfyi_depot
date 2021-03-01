import sys
import itertools
import re
import pprint

def get_entry(fd):
    def to_dict(keyval):
        res = {}
        for (k, v) in keyval:
            assert k not in res
            res[k] = v
        return res

    res = []
    for line in fd:
        if line == "\n":
            yield to_dict(res)
            res = []
        else:
            line = line.rstrip()
            items = line.split(" ")
            for i in items:
                res.append(i.split(":", maxsplit=2))

def val_hgt(hgt):
    m = re.fullmatch(r'([0-9]+)(cm|in)', hgt)
    if m:
        (i, what) = m.group(1,2)
        i = int(i)
        if what == "cm":
            return i >= 150 and i <= 193
        elif what == "in":
            return i >= 59 and i <= 76
        else:
            return False

required_fields = [
    { "name": "byr",
      "check": lambda s: int(s) >= 1920 and int(s) <= 2002
    },
    { "name": "iyr",
      "check": lambda s: int(s) >= 2010 and int(s) <= 2020
    },
    { "name": "eyr",
      "check": lambda s: int(s) >= 2020 and int(s) <= 2030,
    },
    { "name": "hgt",
      "check": lambda s: val_hgt(s)
    },
    { "name": "hcl",
      "check": lambda s: re.fullmatch(r'#[0-9a-f]{6}', s)
    },
    { "name": "ecl",
      "check": lambda s: re.fullmatch(r'amb|blu|brn|gry|grn|hzl|oth', s)
    },
    { "name": "pid",
      "check": lambda s: re.fullmatch(r'[0-9]{9}', s)
    },
    # we should treat it as not required
    # "cid"
]

required_dict = {}
for f in required_fields:
    required_dict[f["name"]] = f

def validate(keyval):
    if keyval[0] not in required_dict:
        return { "ok": keyval }
    if required_dict[keyval[0]]["check"](keyval[1]):
        return { "ok": keyval }
    else:
        return { "validation": keyval }

def all_fields(entry):
    missing = []
    for r in required_dict:
        if r not in e:
            missing.append(r)
    if missing == []:
        return { "ok": entry }
    else:
        return { "missing": missing }

count=0
for e in get_entry(sys.stdin):
    a = all_fields(e)
    if a.get("ok", False):
        res = {}
        bad = False
        for keyval in e.items():
            r = validate(keyval)
            if r.get("validation", False):
                bad = True
            res[keyval[0]] = r
        if bad:
            pprint.pprint({ "validation": res })
        else:
            pprint.pprint({ "ok": e })
            count = count+1
    else:
        pprint.pprint(a)

print(count)
