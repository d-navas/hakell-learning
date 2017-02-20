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
            (fromGregorian 1911 5 1)
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






