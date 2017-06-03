{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Ch21Exercises where

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Chapter 21 Exercises
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- Traversable Instances
-- Write a Traversable instance for the datatype provided,
-- filling in any required superclasses.

-- 1. Write a Traversable instance for Identity
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

-- 2. Write a Traversable instance for Constant
newtype Constant a b = Constant { getConstant :: a }




main :: IO ()
main = putStrLn "works..."
