from collections import Counter

def is_palindrome(x):
    return len([count for _, count in Counter(x).items() if count % 2 == 1]) <= 1


xs = [("civic", True),
      ("ivicc", True),
      ("civil", False),
      ("livci", False)]

for x, expected in xs:
    result = is_palindrome(x)
    print(x)
    print(result)
    assert result == expected
    print("Success!")
