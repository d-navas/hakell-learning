{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Ch18Exercises where

import Control.Monad
import Control.Applicative
import Data.Monoid

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 18.7: Chapter Exercises
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
-- ~~~~~~ 1.
data Nope a = NopeDotJpg
 
instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg
 
instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg
 
instance Monad Nope where
  return = pure
  NopeDotJpg >>= _ = NopeDotJpg
 
-- ~~~~~~ 2.
data Either' b a = Left' a | Right' b
 
instance Functor (Either' b) where
  fmap f (Left' a) = Left' (f a)
  fmap _ (Right' b) = Right' b
 
instance Monoid b => Applicative (Either' b) where
  pure _ = Right' mempty
  Right' b <*> _ = Right' b
  _ <*> Right' b = Right' b
  Left' f <*> Left' a =  Left' (f a)

instance Monoid b => Monad (Either' b) where
  return = pure
  Right' b >>= _ = Right' b
  Left' a >>= f = f a

-- 3.
newtype Identity a = Identity a deriving (Eq, Ord, Show)
 
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
 
instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)
 
instance Monad Identity where
  return = pure
  Identity a >>= f = f a
 
-- 4.
data List a = Nil | Cons a (List a)
 
instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure a = Cons a Nil
  (Cons f Nil) <*> (Cons a Nil) = Cons (f a) Nil
  Cons f fs <*> Cons a as = Cons (f a) (fs <*> as)
  _ <*> Nil = Nil
  Nil <*> _ = Nil
 
instance Monad List where
  return = pure
  Nil >>= _ =  Nil
  as >>= f = join $ fmap f as
 
-- 5: Write the following functions using the methods provided by Monad and Functor.
-- Using identity and composition is fine but it has to typecheck with types provided.
 
-- A:
j :: Monad m => m (m a) -> m a
j = join
 
-- B:
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap
 
-- C:
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftA2
 
-- D:
a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)
 
-- E: You'll need recursion for this one
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = do
  x' <- f x
  fmap ((:) x') (meh xs f)
 
-- F: Hint: reuse "meh"
flipType :: Monad m => [m a] -> m [a]
flipType = flip meh id


-- main
main :: IO ()
main = putStrLn "Ch-18: The Monads"

