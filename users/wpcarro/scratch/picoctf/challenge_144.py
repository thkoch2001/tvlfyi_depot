def rotate_alpha(x, n):
    def rotate_char(c, n):
        offset = 'A' if c.isupper() else 'a'
        return chr((ord(c) - ord(offset) + n) % 26 + ord(offset))
    return "".join([rotate_char(c, n) if c.isalpha() else c for c in x])

xs = [
    "cvpbPGS{arkg_gvzr_V'yy_gel_2_ebhaqf_bs_ebg13_Ncualgvd}",
]
for x in xs:
    print(rotate_alpha(x, 13))
