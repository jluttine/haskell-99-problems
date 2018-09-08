module Ex06 where

-- |Find out whether a list is a palindrome. A palindrome can be read forward or
-- backward; e.g. (x a m a x).
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = compareWithReversed xs rs n
  where
    (rs, n) = reverseWithLength xs

-- |Reverse a list and calculate its length
reverseWithLength :: Integral n => [a] -> ([a], n)
reverseWithLength xs = reverseWithLength' xs [] 0
  where
    reverseWithLength' :: Integral n => [a] -> [a] -> n -> ([a], n)
    reverseWithLength' [] rs n = (rs, n)
    reverseWithLength' (y:ys) rs n = reverseWithLength' ys (y:rs) (n+1)

-- |Compare a list with its reverse given also the length. This comparison makes
-- use of the length to avoid making unnecessary comparison of between the
-- latter halves of the lists.
compareWithReversed :: (Eq a, Integral n) => [a] -> [a] -> n -> Bool
compareWithReversed (y:ys) (r:rs) n
  | n <= 0 = True
  | y == r = compareWithReversed ys rs (n-2)
  | otherwise = False
compareWithReversed [] [] _ = True
compareWithReversed _ _ _ = False

-- |Improved and simplified solution which also only makes half as many
-- comparisons as there are elements in the list
isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' xs = p [] xs xs
   where
     p rev (y:ys) (_:_:zs) = p (y:rev) ys zs
     p rev (_:ys) [_] = rev == ys
     p rev ys [] = rev == ys
     p _ _ _ = error "this shouldn't happen"
