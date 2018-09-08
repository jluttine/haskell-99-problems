-- |Find the last element of a list.
myLast :: [a] -> a
myLast (x : []) = x
myLast (_ : xs) = myLast xs
myLast [] = error "can't find the last element of an empty list"
