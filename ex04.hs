module Ex04 where

-- |Find the number of elements of a list.
myLength :: Integral n => [a] -> n
myLength xs = run xs 0
  where
    run [] n = n
    run (_:ys) n = run ys (n+1)

-- Just for comparison, here's a bad solution which doesn't use tail recursion,
-- thus leading to stack overflow on long lists.
myLengthBad :: Integral n => [a] -> n
myLengthBad [] = 0
myLengthBad (_:xs) = 1 + (myLengthBad xs)
