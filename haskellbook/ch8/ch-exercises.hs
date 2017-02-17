module ChExercises where

import Data.List (intersperse)

-- write a function that recursively sums all numbers from 1 to n
sumAllNums :: (Eq a, Num a) => a -> a
sumAllNums 0 = 0
sumAllNums n = n + (sumAllNums (n - 1))

-- write a function that multiplies two integral numbers using
-- recursive summation.
mult :: (Integral a) => a -> a -> a
mult a 0 = 0
mult a b = a + (mult a (b - 1))


-- Fixing dividedBy
myAbs :: Integral a => a -> a
myAbs a = if a < 0 then (-1) * a else a

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | d == 0       = error "cannot divide by zero"
      | n < myAbs(d) = ((if d < 0 then (-1) * count else count), n)
      | otherwise    = go (n - myAbs(d)) d (count + 1)

{- McCarthy 91 function -}
mc91 :: (Num a, Ord a) => a -> a
mc91 a =
  case a > 100 of
    True  -> a - 10
    False -> mc91 . mc91 $ (a + 11)

{- NUMBERS INTO WORDS -}
digitToWord :: Int -> String
digitToWord n = case n of
  0 -> "zero"
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"
  _ -> error "must provide int"

digits :: Int -> [Int]
digits 0 = []
digits n = (mod n 10) : digits (div n 10)

wordNumber :: Int -> String
wordNumber n = concat . intersperse "-" . reverse $ map digitToWord (digits n)

