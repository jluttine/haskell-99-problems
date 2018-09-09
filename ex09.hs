module Ex09 where

-- |Pack consecutive duplicates of list elements into sublists. If a list
-- |contains repeated elements they should be placed in separate sublists.
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x:group) : pack rest
  where
    (group, rest) = findGroup xs
    findGroup [] = ([], [])
    findGroup ys@(y:yt)
      | x == y = (y:group', rest')
      | otherwise = ([], ys)
        where (group', rest') = findGroup yt

-- With foldr. Note, this evaluates the whole list, not element by element.
-- Can't be used for infinite lists. That's because combine function needs the
-- first element of the result accumulated thus far. Applying this recursively
-- that each element depends on the next, you'll need all elements evaluated.
pack2 :: Eq a => [a] -> [[a]]
pack2 = foldr combine []
  where
    combine x [] = [[x]]
    combine x ((y:ys):yss)
      | x == y = (x:y:ys):yss
      | otherwise = [x]:(y:ys):yss
    combine _ ([]:_) = error "shouldn't happen"
