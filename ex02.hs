-- |Find the last but one element of a list.
myButLast :: [a] -> a
myButLast (x : _ : []) = x
myButLast (_ : []) = error "can't find the last but one element of a list with one element"
myButLast (_ : xs) = myButLast xs
myButLast [] = error "can't find the last but one element of an empty list"
