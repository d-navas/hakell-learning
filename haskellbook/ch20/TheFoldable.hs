{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- {-# MINIMAL foldMap | foldr #-}

module TheFoldable where

import Data.Monoid
import Data.Foldable

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 20.1: Foldable
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 20.2: The Foldable Class
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 20.3: Revenge of the Monoids
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- class Foldable t where
--   fold    :: Monoid m => t m -> m
--   foldMap :: Monoid m => (a -> m) -> t a -> m

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 20.4: Demonstrating Foldable Intances
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
newtype Identity a = Identity a

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

-- Maybe
data Optional a = Nada | Yep a deriving (Eq, Show)

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep a) = f a z
  foldl _ z Nada = z
  foldl f z (Yep a) = f z a
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 20.5: Some Basic Derived Operations
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- toList :: t a -> [a]
-- Test whether the structure is empty
-- null :: t a -> Bool
-- elem :: Eq a => a -> t a -> Bool

-- Exercises:
-- Implement in term of foldMap or foldr

-- 1.
sum :: (Foldable t, Num a) => t a -> a
sum = undefined

-- 2.
product :: (Foldable t, Num a) => t a -> a
product = undefined

-- 3.
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem = undefined

-- 4.
minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = undefined

-- 5.
maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = undefined

-- 6.
null :: Foldable t => t a -> Bool
null = undefined

-- 7.
length :: Foldable t => t a -> Int
length = undefined

-- 8. Some say this is all Foldable amounts to
toList :: Foldable t => t a -> [a]
toList = undefined

-- 9. Hint: use foldMap
fold :: (Foldable t, Monoid m) => t m -> m
fold = undefined

-- 10. Define foldMap in terms of foldr
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap = undefined


-- main
main :: IO ()
main = putStrLn "...works..."

