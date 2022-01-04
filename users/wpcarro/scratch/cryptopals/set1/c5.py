def encrypt_repeating_key(x, key):
    result = b""
    for i in range(len(x)):
        b = ord(x[i]) ^ ord(key[i % len(key)])
        result += b.to_bytes(1, 'big')
    return result.hex()

cleartext = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
expected = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"

run_tests = False
if run_tests:
    ciphertext = encrypt_repeating_key(cleartext, "ICE")
    print(ciphertext)
    assert ciphertext == expected
    print("Success!")
