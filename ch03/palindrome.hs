palindrome xs = xs ++ reverse xs

isPalindrome xs = front == reverse back
  where len = length xs
        half = div len 2
        front = take half xs
        back = if odd len
               then drop (half + 1) xs
               else drop half xs
