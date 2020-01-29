
# swap :: Int -> Int -> [Char] -> IO ()
def swap(ia, iz, xs):
    # handle swap when ia == iz
    assert ia <= iz
    xs[ia], xs[iz] = xs[iz], xs[ia]
    

# reverse :: [Char] -> IO ()
def reverse(xs):
    ia = 0
    iz = len(xs) - 1

    while ia <= iz:
        swap(ia, iz, xs)
        ia += 1
        iz -= 1

x = list("superduperpooper")
reverse(x)
print(x)
print("Tests pass")
