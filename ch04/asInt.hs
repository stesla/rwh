import Data.Char (digitToInt,isDigit)

asInt :: String -> Int
asInt "" = error "no digits"
asInt ('-':digits) = -asInt digits
asInt digits = let (_,acc) = foldr step (0,0) digits in acc
  where step digit _ | not (isDigit digit) = error "not a digit"
        step digit (place,acc) = let place' = place + 1
                                     acc' = acc + 10^place * digitToInt digit
                                 in if acc' < acc
                                    then error "overflow"
                                    else (place',acc')
        
type ErrorMessage = String
asInt_either "" = Left "no digits"
asInt_either ('-':digits) = case asInt_either digits of
  Right result -> Right (-result)
  err -> err
asInt_either digits = let (_,acc) = foldr step (0,Right 0) digits in acc
  where step digit (place, _) | not (isDigit digit) = (place, Left "not a digit")
        step digit (place, Right acc) = let place' = place + 1
                                            acc' = acc + 10^place * digitToInt digit
                                        in if acc' < acc
                                           then (place, Left "overflow")
                                           else (place', Right acc')
        step _ acc = acc

