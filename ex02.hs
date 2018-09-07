module Ex02 where

-- |Find the last but one element of a list.
myButLast :: [a] -> a
myButLast (x : _ : []) = x
myButLast (_ : _ : xs) = myButLast xs
myButLast (_ : []) = error "can't find the last but one element of a list with one element"
myButLast [] = error "can't find the last but one element of an empty list"
