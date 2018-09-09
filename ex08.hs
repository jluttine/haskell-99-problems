module Ex08 where

-- |Eliminate consecutive duplicates of list elements.
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = reverse (compress' [x] xs)
  where
    compress' ys [] = ys
    compress' (y:ys) (v:vs)
      | y == v = compress' (y:ys) vs
      | otherwise = compress' (v:y:ys) vs
    compress' [] _ = error "this shouldn't happen"

-- With foldr
compress2 :: Eq a => [a] -> [a]
compress2 [] = []
compress2 xs = foldr prepend [] xs
  where
    prepend v (y:ys)
      | y == v = y:ys
      | otherwise = v:y:ys
    prepend v [] = [v]

-- Simpler and faster solution from model answers
compress3 :: Eq a => [a] -> [a]
compress3 (x:ys@(y:_))
  | y == x = compress3 ys
  | otherwise = x : (compress3 ys)
compress3 xs = xs
