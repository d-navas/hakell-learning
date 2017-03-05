{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module AlgebraicDatatypes where

import Data.Int
import Data.Char
import Data.List

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Algebraic Datatypes
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- Introduction to datatypes that Hskell offers

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Data Declarations Review
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Data Constructors and Values
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data DogueDeBordeaux doge = DogueDeBordeaux doge

data Doggies a = Husky a | Mastiff a deriving (Eq, Show)

-- 2. What is the kind of Doggies?
-- * -> *

-- 3. What is the kind of Doggies String?
-- Doggies String :: *

-- 4. What is the type of Husky 10?
-- Husky 10 :: Num a => Doggies a

-- 5. What is the type of Husky (10 :: Integer)?
-- Husky (10 :: Integer) :: Doggies Integer

-- 6. What is the type of Mastiff "Scooby Doo"?
-- Mastiff "Scooby Doo" :: Doggies [Char]

-- 7. Is DogueDeBordeaux a type constructor or a data constructor?
-- Both

-- 8. What is the type of DogueDeBordeaux?
-- DogueDeBordeaux :: doge -> DogueDeBordeaux doge

-- 9. What is the type of DogueDeBordeaux "doggie!"
-- DogueDeBordeaux "doggie" :: DogueDeBordeaux [Char]


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Exercises: Vehicles
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data Price = Price Integer deriving (Eq, Show)
data Manufacturer =
      Mini
    | Mazda
    | Tata
    deriving (Eq, Show)

data Airline =
      PapuAir
    | CatapultsR'Us
    | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle =
      Car Manufacturer Price
    | Plane Airline
    deriving (Eq, Show)

myCar    = Car Mini (Price 14000)
urCar    = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge     = Plane PapuAir

-- 1. What is the type of myCar?
-- myCar :: Vehicle

-- 2. Difine the functions
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ ) = True
isPlane _          = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- 3. Write a function that tells the manufacturer
getManu :: Vehicle -> Maybe Manufacturer
getManu (Car a _) = Just a
getManu _         = Nothing

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Data Constructor Arities
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Reading...

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- What Makes These Datatypes Algebraic
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Reading...

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Exercises: Cardinality
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 1. Answer 1
data PugType = PugData

-- 2. Answer 3
data Airline1 =
  PapuAir1
    | CatapultsR'Us1
    | TakeYourChancesUnited1

-- 3. Cardinality of Int16
-- 4. What's the connection between the 8 in Int8 and that types' cardinality of 256?
-- 2 ^ 8

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Simple Datatypes with Nullary Data Constructors
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Unary Constructors
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- newtype
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany (n, s) = n > 42

instance TooMany (Int, Int) where
  tooMany (n, s) = (n + s) > 42

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (n, n') = tooMany (n + n')

-- ~~~
newtype Goats =
  Goats Int deriving (Eq, Show, TooMany)

-- ~~~
newtype Cows =
  Cows Int deriving (Eq, Show)


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- SUM Types: Exercises
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 1.
data BigSmall =
      Big Bool
    | Small Bool
    deriving (Eq, Show)

  -- cardinality 4

-- 2.
data NumberOfBool =
    Numba Int8
  | BoolyBool Bool
  deriving (Eq, Show)

  -- cardinality: 256 + 2

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Product Types
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data QuantumBool = QuantumTrue
                 | QuantumFalse
                 | QuantumBoth deriving (Eq, Show)

data TwoQs = MKTwoQs QuantumBool QuantumBool deriving  (Eq, Show)
-- Product type. Cardinality (cardinality of QuantumBool * cardinality of QuantumBool)
-- 3 * 3 = 9

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Record Syntax
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data Person =
  Person { name :: String
         , age :: Int }
         deriving (Eq, Show)

papu = Person "Papu" 5
-- > name papu -> "Papu"
-- > age papu  -> 5

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Normal Form
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--data Fiction    = Fiction deriving Show
--data NonFiction = NonFiction deriving Show

{- data BookType = FictionBook Fiction
              | NonFictionBook NonFiction
              deriving Show -}

type AuthorName = String

--data Author = Author (AuthorName, BookType)

-- Apply distributive property and rewrite Author in normal form
data Author = Fiction AuthorName
            | NonFiction AuthorName
            deriving (Eq, Show)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Exercises: How Does Your Garden Grow
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 1.
{-data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show -}

type Gardener = String

-- data Garden = Garden Gardener FlowerType deriving Show

  -- Normal form of Garden?
data Garden = Gardenia Gardener
              | Daisy Gardener
              | Rose Gardener
              | Lilac Gardener
              deriving Show

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 11.3: Constructing and Deconstructing Values
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data Id a        = MkId a deriving (Eq, Show)
data Sum a b     = First a | Second b deriving (Eq, Show)
data GuessWhat   = Chickenbutt deriving (Eq, Show)
data Product a b = Product a b deriving (Eq, Show)

data RecordProduct a b = RecordProduct { pfirst  :: a
                                       , psecond :: b }
                                       deriving (Eq, Show)

{- 11.3: Sum and Product -}
newtype NumCow = NumCow Int deriving (Eq, Show)
newtype NumPig = NumPig Int deriving (Eq, Show)

data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)


type Farmhouse' = Product NumCow NumPig

newtype NumSheep  = NumSheep Int deriving (Eq, Show)
data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep deriving (Eq, Show)

type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

{- 11.3: Constructing Values -}

record1 :: RecordProduct Int String
record1 = RecordProduct 2 "dani"

record2 :: RecordProduct Int String
record2 = RecordProduct { pfirst=22, psecond="dani" }

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 11.4: Function Type is Exponential
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data Quantum = Yes
             | No
             | Both
             deriving (Eq, Show)

-- Exponentiation in what order?
convert :: Quantum -> Bool
convert = undefined
 -- Bool = 2 cardinality, Quantum = 3 cardinality
 -- Implementations = 2 ^ 3 = 8

{- 11.4: Exercises: The Quad -}
-- Determine how many unique inhabitants

-- 1.
data Quad = One | Two | Three | Four deriving (Eq, Show)
eQuad :: Either Quad Quad
eQuad = undefined
-- Quad + Quad = 4 + 4 = 8 unique inhabitants

-- 2.
proQuad :: (Quad, Quad)
proQuad = undefined
-- 4 * 4 = 8

-- 3.
funcQuad :: Quad -> Quad
funcQuad = undefined
-- 4 ^ 4 = 256

-- 4.
prodTBool :: (Bool, Bool, Bool)
prodTBool = undefined
-- 2 * 2 * 2 = 8

-- 5.
gTwo :: Bool -> Bool -> Bool
gTwo = undefined
-- (2 ^ 2) ^ 2 = 16

-- 6. Hint: 5 digit number
fTwo :: Bool -> Quad -> Quad
fTwo = undefined
-- (4 ^ 4) ^ 2 = 65536


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 11.15: Higher-Kinded datatypes
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 11.16: Lists are Polymorphic
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Infix data constructor
data Product1 a b = a :&: b deriving (Eq, Show)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 11.17: Binary Tree
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

  {- 11.17: Inserting into Trees -}
insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a  = Node (insert' b left) a right
  | b > a  = Node left a (insert' b right)

-- Write a mapTree function
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

-- Acceptance test for mapTree
mapOkay =
  if mapTree (+1) testTree' == mapExpected
     then print "yup okay!"
     else error "test failed!"

  {- 11.17: Convert Binary Trees to Lists -}
testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

-- Pre Order
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : preorder left ++ preorder right

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
     then putStrLn "Preorder fine!"
     else putStrLn "Bad news bears."

-- In Order
inorder :: BinaryTree a -> [a]
inorder (Node left a right) = preorder left ++ [a] ++ preorder right

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
     then putStrLn "Inorder fine!"
     else putStrLn "Bad news bears."

-- Post Order
postorder :: BinaryTree a -> [a]
postorder (Node left a right) = preorder left ++ preorder right ++ [a]

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
     then putStrLn "Postorder fine!"
     else putStrLn "postorder failed check"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

  {- 11.17: Write foldr for BinaryTree -}
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f b Leaf = b
foldTree f b (Node left a right) = f a $ foldTree f (foldTree f b left) right

-- As Patterns
------------------------
isSubsequence :: (Eq a) => [a] -> [a] -> Bool
isSubsequence [] _ = True
isSubsequence _ [] = False
isSubsequence xss@(x:xs) (y:ys)
  | x == y    = isSubsequence xs ys
  | otherwise = isSubsequence xss ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords s = map (\w@(c:cs) -> (w, (toUpper c) : cs)) $ words s

capWord :: String -> String
capWord (c:cs) = (toUpper c) : cs

capPara :: String -> String
capPara s
  | s == [] = []
  | otherwise = intercalate " " $ shouldCapWord (words s) True
  where
    shouldCapWord :: [String] -> Bool -> [String]
    shouldCapWord [] _ = []
    shouldCapWord (x:xs) True = capWord x : shouldCapWord xs (elem '.' x)
    shouldCapWord (x:xs) False = x : shouldCapWord xs (elem '.' x)

--------------------
-- Phone Exercise --
--------------------
data DaPhone = DaPhone

convo :: [String]
convo =
  ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol lol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Haha thanks just making sure rofl ur turn"]

type Digit = Char
type Presses = Int

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps = undefined

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead = undefined

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = undefined

mostPopularLetter :: String -> Char
mostPopularLetter = undefined

coolestLtr :: [String] -> Char
coolestLtr = undefined

coolestWord :: [String] -> String
coolestWord = undefined

--------------------
-- Hutton's Razor --
--------------------
  -- 1. write "eval" function which reduces an expression to a final sum.
data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit a) = a
eval (Add e1 e2) = eval e1 + eval e2

  -- 2. write a printer for the expressions
printExpr :: Expr -> String
printExpr (Lit a) = show a
printExpr (Add e1 e2) = printExpr e1 ++ " + " ++ printExpr e2


a1 = Add (Lit 9001) (Lit 1)
a2 = Add a1 (Lit 2001)
a3 = Add (Lit 1) a2



