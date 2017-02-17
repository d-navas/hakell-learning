module Recursion where

-- ex 1
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial(n - 1)

-- ex 2
applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f . applyTimes (n - 1) f $ b

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes times n = applyTimes times (+1) n


{- Topic: BOTTOM -}
-- let x = x in x
-- infinite loop, why?

{- Topic: FIBONACCI NUMBERS -}
fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

{- Topic: INTEGRAL DIVISION FROM SCRATCH -}
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | n < d     = (count, n)
      | otherwise = go (n - d) d (count + 1)

