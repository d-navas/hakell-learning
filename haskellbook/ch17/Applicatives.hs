module Applicatives where

import Data.List (elemIndex)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- 17.2: Defining Applicative
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- So, every type that can have an Applicative instance must also have a Functor instance
-- class Functor f => Applicative f where
--   pure  :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

-- Core Control.Applicative functions
-- liftA :: Applicative f => (a -> b)
--           -> f a
--           -> f b

-- liftA2 :: Applicative f => (a -> b -> c)
--           -> f a
--           -> f b
--           -> f c

-- liftA3 :: Applicative f => (a -> b -> c -> d)
--           -> f a
--           -> f b
--           -> f c
--           -> f d

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 17.3: Functor vs Applicative
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- fmap (+1) [1..3] is equivalent to pure (+1) <*> [1..3]

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 17.4: Applicative Functors are monoidal functors
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- mappend :: Monoid a => a -> a -> a

{- Show me the monoids -}

{- Tuple Monoid nad Applicative side by side -}
-- instance (Monoid a, Monoid b) => Monoid (a, b) where
--   mempty = (mempty, mempty)
--   (a, b) `mappend` (a', b') = (a `mappend` a', b `mappend` b')

-- instance Monoid a => Applicative ((,) a) where
--   pure x = (mempty, x)
--   (u, f) <*> (v, x) = (u `mappend` v, f x)

{- Maybe Monoid and Applicative -}
-- instance Monoid a => Monoid a (Maybe a) where
--   mempty = Nothing
--   mappend m Nothing = m
--   mappend Nothing m = m
--   mappend (Just a) (Just a') = Just (a `mappend` a')

-- instance Applicative Maybe where -- implicit Monoid (see it?)
--   pure = Just
--   Nothing <*> _ = Nothing
--   Just f <*> Just a = Just (f a)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 17.5: Applicative in Use
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- f x = lookup x [(3, "hello"), (4, "julie"), (5, "kbai")]
-- g y = lookup y [(7, "sup?"), (8, "chris"), (9, "aloha")]
-- h z = lookup z [(2, 3), (5, 6), (7, 8)]
-- m x = lookup x [(4, 10), (8, 13), (1, 9001)]

{- Exercises -}
-- Make following expressions typecheck

-- 1.
added :: Maybe Integer
added = (+3) <$> lookup 3 (zip [1, 2, 3] [4, 5, 6])

-- 2.
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3.
x :: Maybe Int
x = elemIndex 3 [1..5]

h :: Maybe Int
h = elemIndex 4 [1..5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*>  h

-- 4.
xs :: [Integer]
xs = [1..3]

ys :: [Integer]
ys = [4..6]

l :: Maybe Integer
l = lookup 3 $ zip xs ys

k :: Maybe Integer
k = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fst $ sum <$> (,) l k




main :: IO ()
main = putStrLn "Ch-17: Applicatives"