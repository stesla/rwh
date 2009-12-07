data Tree a = Tree a (Maybe (Tree a)) (Maybe (Tree a))
                 deriving Show

data Node a = Empty
            | Node a (Node a) (Node a)
              deriving Show

height :: Node a -> Int                      
height Empty = 0
height (Node _ left right) = 1 + max (height left) (height right)
