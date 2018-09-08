module Ex05 where

-- |Reverse a list.
myReverse :: [a] -> [a]
myReverse xs = acc xs []
  where
    acc [] rs = rs
    acc (y:ys) rs = acc ys (y:rs)
