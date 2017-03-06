module SignalingAdversity where

import Data.List

-- 12.2 How I learned to stop worrying and love Nothing

-- 12.3 Bleating either
type Name = String
type Age  = Integer
type ValidatePerson a = Either [PersonInvalid] a

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   deriving (Eq, Show)

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = case age >= 0 of
                True  -> Right age
                False -> Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = case name /= "" of
                  True  -> Right name
                  False -> Left [NameEmpty]

makePerson :: Name -> Age -> ValidatePerson Person
makePerson name age = makePerson' (nameOkay name) (ageOkay age)
  where
    makePerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
    makePerson' (Right nameOkay) (Right ageOkay) = Right (Person nameOkay ageOkay)
    makePerson' (Left badName) (Left badAge)     = Left (badName ++ badAge)
    makePerson' (Left badName) _ = Left badName
    makePerson' _ (Left badAge)  = Left badAge

-- prelude> makePerson "" (-1)
-- prelude> Left [NameEmpty, AgeTooLow]

{-~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
12.4: Kinds, a thousand stars in your types
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-}

-- Data Constructors are functions

{-~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
12.5: Chapter Exercises
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-}

-- Unfolds
listOf10 = take 10 $ unfoldr (\b -> Just (b, b+1)) 0
-- prelude> [0,1,2,3,4,5,6,7,8,9]

mehSum :: Num a => [a] -> a
mehSum xs = go 0 xs
  where go :: Num a => a -> [a] -> a
        go n [] = n
        go n (x:xs) = go (n+x) xs

niceSumm :: Num a => [a] -> a
niceSumm = foldl' (+) 0

mehProduct :: Num a => [a] -> a
mehProduct xs = go 1 xs
  where go :: Num a => a -> [a] -> a
        go n [] = n
        go n (x:xs) = go (n*x) xs

niceProduct :: Num a => [a] -> a
niceProduct = foldl' (*) 1

-- Finally something other than a list
-- Binary Tree exercises. Skipped them for the sake of time

