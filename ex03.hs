module Ex03 where

-- |Find the K'th element of a list. The first element in the list is number 1.
elementAt :: Integral n => [a] -> n -> a
elementAt [] _ = error "index out of bounds"
elementAt (x:_) 1 = x
elementAt (_:xs) n =
  if n > 1
  then elementAt xs (n-1)
  else error "index out of bounds"
