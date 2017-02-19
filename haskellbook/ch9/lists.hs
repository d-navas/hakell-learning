module Lists where

{- The List datatype -}
-- data [] a = [] | a : [a]

{- Pattern matching on lists -}


{- EXERCISES: THY FEARFUL SYMMETRY -}

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 1
myWords :: String -> [String]
myWords x =
  case x of
    []       -> []
    (' ':xs) -> myWords xs
    _        -> takeWhile (/= ' ') x : myWords (dropWhile (/= ' ') x)

-- Example:
  -- Main> myWords "all i wanna do is have some fun"
  -- ["all","i","wanna","do","is","have","some","fun"]

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 2: write a function that takes a string and returns a list of strings
firstSen  = "Tyger tyger, burning bright\n"
secondSen = "in the forests of the night\n"
thirdSen  = "what immortal hand or eye\n"
fourthSen = "could frame thy fearful symmetry"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> Char -> [String]
myLines x c =
  case x of
    []     -> []
    (z:zs) ->
      if z == c
         then myLines zs c
         else takeWhile (/= c) x : myLines (dropWhile (/= c) x) c

shouldEqual =
  [ "Tyger tyger, burning bright"
  , "in the forests of the night"
  , "what immortal hand or eye" 
  , "could frame thy fearful symmetry"
  ]

main :: IO ()
main = print $ "Are they equal? " ++ show (myLines sentences '\n' == shouldEqual)


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- L I S T   C O M P R E H E N S I O N S
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
squaredRem0 = [x^2 | x <- [1..10], rem x 2 == 0]

mySqr = [x^2 | x <- [1..5]]

-- multiple generators
powered = [x^y | x <- [1..5], y <- [2,3]]

lessThan200 = [x^y | x <- [1..10], y <- [2, 3], x^y < 200]

listOfTuples1 = [(x, y) | x <- [1, 2, 3], y <- [6, 7]]

listOfTuples2 = [(x, y) | x <- [1, 2, 3], y <- ['a', 'b']]

-- E X E R C I S E S: Comprehend thy Lists
-- NOTE: didn't do it, too easy

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- List Comprehensions with Strings --
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
threeLetterAcron = [x | x <- "Three Letter Acronym", elem x ['A'..'Z']]

-- E X E R C I S E S: Square Cube
-- Given the following:
mySqr1 = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

  --1: write an expression taht will make tuples of the outputs of mySqr and myCube
  --2: alter the expression so that it only uses the x and y values that are less than 500

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Spines and Non-strict Evaluation
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- Skipped it. Mostly explaining how the compiler evaluates (:) cons


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Exercises: Bottom Madness
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Skipped them. Didn't find them interesting or helpful


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Transforming Lists of Values
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plusOne  = map (+1) [1, 2, 3, 4]
oneMinus = map (1-) [1, 2, 3, 4]
plusOneF = fmap (+1) [1, 2, 3, 4]
oneMinuF = fmap (2*) [1, 2, 3, 4]

  -- Definition of map in Base (double 'p' to avoid conflict)
mapp :: (a -> b) -> [a] -> [b]
mapp _ [] = []
mapp f (x:xs) = f x : map f xs


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Exercises: More Bottoms
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
isAChar = map (\x -> elem x "a") "danilo"
-- [False, True, False, False, False, False]

findMin = map minimum [[1..10], [10..20], [20..30]]
-- [1, 10, 20]


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Filtering Lists of Values
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
findEven = filter even [1..10]

  -- Definition of filter
filterr :: (a -> Bool) -> [a] -> [a]
filterr _ [] = []
filterr pred (x:xs)
  | pred x    = x : filter pred xs
  | otherwise = filter pred xs

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Exercises: Filtering
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
filterArticles = filter (\x -> x /= "a" && x /= "an" && x /= "the") $ words "the brown dog was a goof"

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Zipping Lists
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
zipDefault = zip [1, 2] [4, 5, 6] -- [(1,4), (2,5)]
zipPlus    = zipWith (+) [1, 2, 3] [10, 11, 12] -- [11, 13, 15]

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Exercises: Zipping
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Skipped them. Did not find them useful


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- CHAPTER 9: EXERCISES
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~









