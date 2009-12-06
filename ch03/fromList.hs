data List a = Cons a (List a)
            | Nil
              deriving Show

toList (x:xs) = Cons x (toList xs)
toList [] = Nil

fromList (Cons x xs) = x:(fromList xs)
fromList Nil = []