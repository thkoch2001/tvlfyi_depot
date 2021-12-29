from base64 import b64encode

################################################################################
# Challenge 1
################################################################################

def hex_to_base64(x):
    parsed = bytearray.fromhex(x)
    print(parsed.decode()) # easter egg
    return b64encode(parsed).decode()

run_tests = False
if run_tests:
    actual = hex_to_base64("49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")
    expect = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

    print(actual)
    assert actual == expect
    print("Success!")
