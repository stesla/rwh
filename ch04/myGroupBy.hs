import Data.List (foldl')

myGroupBy f (x:xs) = let (_,group,groups) = foldl' step (x,[x],[]) xs
                     in reverse (map reverse (group:groups))
  where step (y,ys,zs) x | f x y = (y,x:ys,zs)
                         | otherwise = (x,[x],ys:zs)