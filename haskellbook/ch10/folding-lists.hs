module FoldingLists where

import Data.Time

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Bringing You Into The Fold
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Folds as a general concept are called catamorphisms.
-- foldr: fold right

-- Right folds have to traverse the list outside-in, but the
-- folding itself starts from the end of the list.

-- works becuase only the first 2 elems in the array are evaluated
nonStrict = length $ take 2 $ take 4 ([1, 2] ++ undefined)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Fold Left
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Given the list
-- foldl (+) 0 (1 : 2 : 3 : [])
-- foldl associates like so
-- ((0 + 1) + 2) + 3

-- Scans (scanr, scanl) are similar to folds but return a list of all the
-- intermediate stages of the fold.

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Associativity and Folding
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Exercises: Understanding Folds
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Page: 360, 361, 362.
-- Great Exercises on Folds. Do again when revisiting.

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Exercises: Database Processing
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, World!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

-- 1. write a function that filters for DbDate values and returns a list of the UTCTime values inside them.
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate row = foldr f [] row
  where
    f :: DatabaseItem -> [UTCTime] -> [UTCTime]
    f (DbDate date) b = date:b
    f _ b             = b

-- 2. Write a function that filters for DbNumber values and returns a list of the Integer values inside Them
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber row = foldr f [] row
  where
    f :: DatabaseItem -> [Integer] -> [Integer]
    f (DbNumber date) b = date:b
    f _ b             = b

-- 3. Wite a function that gets the most recent date.
-- Skipped it. It's just a matter of re-using filterDbDate and reducing

-- 4. Write a function that sums all the DbNumber values
-- Skipped it. It's just a matter of re-using filterDbNumber and reduce by (+)

-- 5. Write a function that gets the average of the DbNumber values
-- Skipped it. It's just a matter of re-using filterDbNumber and reduce by getting average

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Folding and Evaluation
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The relationship between foldr and foldl is such that:
-- For infinite lists only.
-- foldr f z xs = foldl (flip f) z (reverse xs)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- S c a n s
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{-
Scans, which we have mentioned above, work similarly to maps and
also to folds. Like folds, they accumulate values instead of keeping
the list’s individual values separate. Like maps, they return a list of
results. In this case, the list of results shows the intermediate stages
of evaluation, that is, the values that accumulate as the function is
doing its work
-}

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- CHAPTER 10 Exercises
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 1. Write a function that creates 3-tuples of words and nouns (skipped it)
-- 2. What does the mystery function do? (skipped it)
seekritFunc x =
  div (sum (map length (words x)))
      (length (words x))
-- 3. Rewrite above using fractional division (skipped it)

{- Rewriting functions using folds -}

-- Point free version of &&

-- direct recursion not using if
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) =
  if x == False
     then False
     else myAnd xs

-- direct recursion using &&
myAnd1 :: [Bool] -> Bool
myAnd1 [] = True
myAnd1 (x:xs) = x && myAnd1 xs

-- fold, not point-free in the folding function
myAnd2 :: [Bool] -> Bool
myAnd2 = foldr (\a b -> if a == False then False else b) True

-- fold, both myAnd and the folding function are point-gree now
myAnd3 :: [Bool] -> Bool
myAnd3 = foldr (&&) True

-- DEFINITIONS Page 378

