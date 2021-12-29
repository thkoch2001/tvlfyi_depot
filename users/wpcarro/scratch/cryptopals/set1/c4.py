import c3

content = None
with open('4.txt', 'r') as f:
    c3.decode_cipher
    content = f.read().splitlines()

for line in content:
    try:
        print(c3.decode_cipher(line))
    except:
        continue
