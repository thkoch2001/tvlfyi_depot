module NonStrictnessScratch where

x = undefined
y = "blah"
main = do
  print $ snd (x, x `seq` y)
