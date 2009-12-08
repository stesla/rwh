import Data.Char (isSpace)
  
myAny f xs = foldr step False xs
  where step x False = f x
        step _ _ = True

myCycle xs = foldr step [] [1..]
  where step _ ys = xs ++ ys

myWords string = let (word,words) = foldr step ("",[]) string in word:words
  where step c ("",words) | isSpace c = ("",words)
                          | otherwise = ([c],words)
        step c (word,words) | isSpace c = ("",word:words)
                            | otherwise = (c:word,words)

myUnlines xs = foldr step "" xs
  where step line acc = concat [line, "\n", acc]