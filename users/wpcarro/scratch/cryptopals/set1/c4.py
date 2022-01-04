import c3

content = None
with open('4.txt', 'r') as f:
    content = f.read().splitlines()
if not content:
    raise Error("Need content to proceed")

xs = []
for line in content:
    try:
        x = c3.decode_cipher(line)
        if x: xs.append(x)
    except:
        continue

freqs = c3.frequency_table()
print(max(xs, key=lambda x: c3.score(x, freqs)))

################################################################################
# Answer
################################################################################
"Now that the party is jumping"
