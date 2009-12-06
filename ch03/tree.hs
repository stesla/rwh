data Tree a = Tree {left :: Maybe (Tree a), right :: Maybe (Tree a)}
            | Node a 
            deriving Show