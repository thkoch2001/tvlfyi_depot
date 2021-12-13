THOUSAND = int(1e3)
MILLION = int(1e6)
BILLION = int(1e9)
TRILLION = int(1e12)

facts = {
    1: "One",
    2: "Two",
    3: "Three",
    4: "Four",
    5: "Five",
    6: "Six",
    7: "Seven",
    8: "Eight",
    9: "Nine",
    10: "Ten",
    11: "Eleven",
    12: "Twelve",
    13: "Thirteen",
    14: "Fourteen",
    15: "Fifteen",
    16: "Sixteen",
    17: "Seventeen",
    18: "Eighteen",
    19: "Nineteen",
    20: "Twenty",
    30: "Thirty",
    40: "Forty",
    50: "Fifty",
    60: "Sixty",
    70: "Seventy",
    80: "Eighty",
    90: "Ninety",
    100: "Hundred",
    THOUSAND: "Thousand",
    MILLION: "Million",
    BILLION: "Billion",
    TRILLION: "Trillion",
}

def anglocize(x):
    # ones
    if x >= 0 and x < 10:
        pass

    # tens
    elif x < 100:
        pass

    # hundreds
    elif x < THOUSAND:
        pass

    # thousands
    elif x < MILLION:
        pass

    # millions
    elif x < BILLION:
        pass

    # billion
    elif x < TRILLION:
        pass

    # trillion
    else:
        pass

x = 1234
assert anglocize(x) == "One Thousand, Two Hundred Thirty Four"
