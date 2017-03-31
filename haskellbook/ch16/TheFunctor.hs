module TheFunctor where

import Test.QuickCheck
import Test.QuickCheck.Function

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 16.2: What is a Functor
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 16.3: There's a whole lot of fmap going round
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
data FixMePls a = FixMe | Pls a deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 16.5: Functor Laws
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~

-- Identity
-- fmap id == id

-- Composition
-- fmap (f . g) == fmap f . fmap g

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 16.6: The Good, the Bad, and the Ugly
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
data WhoCares a = ItDoesnt | Matter a | WhatThisIsCalled deriving (Eq, Show)

instance Functor WhoCares where
  fmap _ ItDoesnt = ItDoesnt
  fmap _ WhatThisIsCalled = WhatThisIsCalled
  fmap f (Matter a) = Matter (f a)

-- Law breaking instance. Structure is broken
-- instance Functor WhoCares where
--   fmap _ ItDoesnt = WhatThisIsCalled
--   fmap f WhatThisIsCalled = ItDoesnt
--   fmap f (Matter a) = Matter (f a)

-- Composable functor with extra param in data constructo
data CountingBad a = Heisenberg Int a deriving (Eq, Show)

instance Functor CountingBad where
  fmap f (Heisenberg n a) = Heisenberg n (f a)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 16.7: Commonly Used Functors
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~

-- The functors are stacked and that's a fact
lms = [Just "Ave", Nothing, Just "woohoo"]
replaceWithP = const 'p'
-- > replaceWithP lms --- R: 'p'
-- > fmap replaceWithP lms --- R: "ppp"
-- > (fmap . fmap) replaceWithP lms --- R: [Just 'p', Nothing, Just 'p']
-- > (fmap . fmap . fmap) replaceWithP lms --- R: [Just "ppp", Nothing, Just "pppppp"]

-- ~~~~~~~~~
-- One more round for the P-Funkshun
-- ~~~~~~~~~
replaceWithP' :: b -> Char
replaceWithP' = const 'p'

lms' :: [Maybe String]
lms' = [Just "Ave", Nothing, Just "woohoo"]

-- making the argument more specific
replaceWithP'' :: [Maybe [Char]] -> Char
replaceWithP'' = replaceWithP'

-- Prelude> :t fmap replaceWithP'
-- fmap replaceWithP' :: Functor f => f a -> f Char

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP'

-- we can assert a more specific type
liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 16.8: Transforming the Unapplied Type Argument
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data Two a b = Two a b deriving (Eq, Show)

data Or a b = First a | Second b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Functor (Or a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 16.9: QuickChecking Functor Instances
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = fmap g (fmap f x) == fmap (g . f) x

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

-- Main
main :: IO ()
main = do
  print (replaceWithP'' lms)
  quickCheck $ \x -> functorIdentity (x :: [Int])
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: [Int])
  quickCheck (functorCompose' :: IntFC)


