import Data.List
       
sortLists xs = sortBy increasingLength xs
  where increasingLength xs ys = compare (length xs) (length ys)