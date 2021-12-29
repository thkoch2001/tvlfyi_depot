def fixed_xor(x, y, decode_hex=True, encode_hex=True):
    if decode_hex:
        x = bytearray.fromhex(x)
        y = bytearray.fromhex(y)

    result = bytearray(len(x))

    for i in range(len(x)):
        result[i] = x[i] ^ y[i]

    return result.hex() if encode_hex else result

run_tests = False
if run_tests:
    actual = fixed_xor("1c0111001f010100061a024b53535009181c", "686974207468652062756c6c277320657965")
    expect = "746865206b696420646f6e277420706c6179"

    print(actual)
    assert actual == expect
    print("Success!")
