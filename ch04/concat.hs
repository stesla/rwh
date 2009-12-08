myConcat :: [[a]] -> [a]
myConcat xs = foldr step [] xs
  where step ys acc = ys ++ acc