-- Raises an exception if the list is not long enough
lastButOne xs = if length xs == 2
                then head xs
                else lastButOne (tail xs)
