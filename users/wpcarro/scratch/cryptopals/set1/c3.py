from c2 import fixed_xor
from collections import Counter

def frequency_table():
    with open('alice.txt', 'r') as f:
        chars = {}
        while True:
            l = f.readline()
            if not l: break
            for c in l:
                chars[c] = chars.get(c, 0) + 1
        result = {}
        for c, n in chars.items():
            result[c] = n / len(chars)
        return result

def score(bs, freqs):
    return sum(freqs.get(b, 0) for b in bs)

def decode_cipher(x):
    freqs = frequency_table()

    if not freqs:
        raise Error("Cannot decode cipher without a populated frequency table")

    x = bytearray.fromhex(x)
    num_bytes = len(x)

    mx, result, key = 0, None, None
    for b in range(0, 1 << 8):
        mask = bytearray(b.to_bytes(1, 'big') * num_bytes)
        try:
            y = fixed_xor(x, mask, decode_hex=False, encode_hex=False).decode('ascii')
        except:
            continue
        test = score(y, freqs)
        if test > mx:
            result = y
            mx = test
            key = mask.decode('ascii')
    return result

run_tests = False
if run_tests:
    print(decode_cipher("1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"))

################################################################################
# Answer
################################################################################
"Cooking MC's like a pound of bacon"
