import Data.List

-- |Find the number of elements of a list.
myLength :: Integral n => [a] -> n
myLength xs = run xs 0
  where
    run [] n = n
    run (_:xs) n = run xs (n+1)

-- This is how it is implemented in Prelude and it's faster than the one above
myLength' :: Integral n => [a] -> n
myLength' = foldl' (\c _ -> c+1) 0

-- Just for comparison, here's a bad solution which doesn't use tail recursion,
-- thus leading to stack overflow on long lists.
myLengthBad :: Integral n => [a] -> n
myLengthBad [] = 0
myLengthBad (_:xs) = 1 + (myLengthBad xs)
