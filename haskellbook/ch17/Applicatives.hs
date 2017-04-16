module Applicatives where

-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- 17.2: Defining Applicative
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- So, every type that can have an Applicative instance must also have a Functor instance
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

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



main :: IO ()
main = putStrLn "hello"
