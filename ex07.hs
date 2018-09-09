module Ex07 where

data NestedList a = Elem a | List [NestedList a] deriving Show

-- |Flatten a nested list structure.
flatten :: NestedList a -> [a]
flatten (List xs) = foldr append [] xs
  where
    append l ys = (flatten l) ++ ys
flatten (Elem x) = [x]

-- Another solution. Which one is faster?
flatten' :: NestedList a -> [a]
flatten' l = reverse $ acc [] l
  where
    acc xs (Elem x) = x:xs
    acc xs (List []) = xs
    acc xs (List (y:ys)) = acc (acc xs y) (List ys)
