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
















