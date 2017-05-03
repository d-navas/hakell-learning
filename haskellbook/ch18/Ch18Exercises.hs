{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Ch18Exercises where
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 18.7: Chapter Exercises
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
-- 1.
data Nope a = NopeDotJpg
 
instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg
 
instance Applicative Nope where
  pure _ = NopeDotJpg
  NopeDotJpg <*> _ = NopeDotJpg
  _ <*> NopeDotJpg = NopeDotJpg
 
instance Monad Nope where
  return = pure
  NopeDotJpg >>= _ = NopeDotJpg
 
-- 2.
data Either' b a = Left' a | Right' b
 
instance Functor (Either' b) where
  fmap f (Left' a) = Left' (f a)
  fmap _ (Right' b) = Right' b
 
instance Monoid b => Applicative (Either' b) where
  pure _ = Right' mempty
  Right' b <*> _ = Right' b
  _ <*> Right' b = Right' b
  Left' f <*> Left' a =  Left' (f a)

main :: IO ()
main = putStrLn "Ch-18: The Monads"
