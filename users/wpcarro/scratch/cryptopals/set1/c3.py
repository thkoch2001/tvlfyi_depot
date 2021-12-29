from c2 import fixed_xor
from collections import Counter
import string

alphabet = string.ascii_lowercase + string.ascii_uppercase

def score(bs):
    chars = [b for b in bs if b in alphabet]
    return sum(Counter(chars).values())

def decode_cipher(x):
    x = bytearray.fromhex(x)
    num_bytes = len(x)

    mx, result, key = 0, None, None
    for c in alphabet:
        mask = bytearray(bytes(c, 'ascii') * num_bytes)
        y = fixed_xor(x, mask, decode_hex=False, encode_hex=False).decode('ascii')
        test = score(y)
        if test > mx:
            result = y
            mx = test
            key = mask.decode('ascii')
    return result

run_tests = False
if run_tests:
    print(decode_cipher("1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"))
