myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f (x:xs) | f x = x:myTakeWhile f xs
myTakeWhile _ _ = []

myTakeWhile_foldr f xs = foldr step [] xs
  where step x ys | f x = x:ys
                  | otherwise = []
