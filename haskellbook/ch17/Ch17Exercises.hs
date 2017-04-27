{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Ch17Exercises where

import Control.Applicative

-- 1.
-- []
-- pure :: a -> ? a
-- (<*>) :: ? (a -> b) -> ? a -> ? b

-- pure :: a -> [a]
-- (<*>) :: [(a -> b)] -> [a] -> [b]

-- 2.
-- IO
-- pure :: a -> ? a
-- (<*>) :: ? (a -> b) -> ? a -> ? b

-- pure :: a -> IO a 
-- (<*>) :: IO (a -> b) -> IO a -> IO b

-- 3.
-- (,) a
-- pure :: a -> ? a
-- (<*>) :: ? (a -> b) -> ? a -> ? b

-- pure :: a -> (a, a)
-- (<*>) :: (a, (a -> b)) -> (a, a) -> (a, b)

-- 4.
-- (->) e
-- pure :: a -> (e -> a)
-- (<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)

-- WRITE APPLICATIVE INSTANCES

-- 1.
data Pair a = Pair a a deriving Show

instance Functor Pair where
  fmap = undefined

instance Applicative Pair where
  pure a = Pair a a 
  Pair f f' <*> Pair a a' = Pair (f a) (f' a')

-- 2.
data Two a b = Two a b

instance Functor (Two a) where
  fmap = undefined

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  Two a f <*> Two a' b = Two (a `mappend` a') (f b)

-- 3.
data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap = undefined

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  Three a b f <*> Three a' b' c = Three (a `mappend` a') (b `mappend` b') (f c)

-- 4.
data Three' a b = Three' a b b

instance Functor (Three' a) where
  fmap = undefined

instance Monoid a => Applicative (Three' a) where
  pure b = Three' mempty b b
  Three' a f f' <*> Three' a' b b' = Three' (a `mappend` a') (f b) (f' b')

-- 5.
data Four a b = Four a a a b

instance Functor (Four a) where
  fmap = undefined

instance Monoid a => Applicative (Four a) where
  pure = Four mempty mempty mempty
  Four a a1 a2 f <*> Four b b1 b2 c =
    Four (a `mappend` b) (a1 `mappend` b1) (a2 `mappend` b2) (f c)

-- ~~~~~~~~~~~~~~
-- Combinations

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)



main :: IO ()
main = putStrLn "Ch-17 Exercises"
