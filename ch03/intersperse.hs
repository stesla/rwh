intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ [x] = x
intersperse separator (x:xs) = x ++ separator:intersperse separator xs
