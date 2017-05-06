{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- {-# MINIMAL foldMap | foldr #-}

module TheFoldable where

import Data.Monoid

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 20.1: Foldable
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 20.2: The Foldable Class
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 20.3: Revenge of the Monoids
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
class Foldable t where
  fold    :: Monoid m => t m -> m
  foldMap :: Monoid m => (a -> m) -> t a -> m

main :: IO ()
main = putStrLn "...works..."
