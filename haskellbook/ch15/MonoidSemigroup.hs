module MonoidSemigroup where

import Control.Monad
import Data.Monoid
import Test.QuickCheck

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 15.2: What we talk about when we talk about algebras
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 15.4: How Monoid is defined in Haskell
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 15.5: Examples of using Monoid
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 15.6: Why Integer doesn't have a Monoid
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- val1, val2, and val3 are equivalent
val1 :: Sum Integer
val1 = mappend (Sum 1) (mappend (Sum 2) (Sum 3)) -- Sum {getSum = 6}
res1 = getSum val1 -- 6

val2 :: Sum Integer
val2 = Sum 1 <> Sum 2 <> Sum 3 -- Sum {getSum = 6}
res2 = getSum val2 -- 6

val3 :: Sum Integer
val3 = mconcat [Sum 8, Sum 9, Sum 10] -- Sum {getSum = 27}
res3 = getSum val3 -- 27

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 15.7: Why Bother?
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 15.8: Laws
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 15.10: Reusing algebras by asking for algebras
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- instance Monoid b => Monoid (a -> b)
-- instance (Monoid a, Monoid b) => Monoid (a, b)
-- instance (Monoid a, Monoid b, Monoid c) => Monoid (a, b, c)
data Booly a = False' | True' deriving (Eq, Show)

-- conjunction: just cause
instance Monoid (Booly a) where
  mempty = False' -- this did just so it compiles not the actual mempty!!
  mappend False' _    = False'
  mappend _ False'    = False'
  mappend True' True' = True'

-- Exercise: Optional Monoid
data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada (Only a) = Only a
  mappend (Only a) Nada = Only a
  mappend (Only a) (Only a') = Only (a <> a')
  mappend Nada Nada = Nada

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 15.11: Madness
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' e adv noun adj =
  e <> "! he said " <>
  adv <> " as he jumped into his car " <>
  noun <> " and drove off with his " <>
  adj <> " wife."

madLibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madLibbinBetter' e adv noun adj =
   mconcat [ e
           , "! he said "
           , adv
           , " as he jumped into his car "
           , noun
           , " and drove off with his "
           , adj
           , " wife."]

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 15.12: Better Living Through QuickCheck
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- Proving associativity ~~~~~
monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- Quickchecking Left and Right Identity ~~~~~
monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- Testing QuickCheck's Patience ~~~~~~~
data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [ (1, return Fools)
                        , (1, return Twoo) ]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

-- Exercise: Maybe another Monio ~~~~~~~
-- Write a Monoid instance for Maybe type which doesn't require a Monoid for the contents.
newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' (Only a)) (First' (Only a')) = First' $ Only a
  mappend (First' (Only a)) (First' Nada) = First' $ Only a
  mappend (First' Nada) (First' (Only a')) = First' $ Only a'
  mappend (First' Nada) (First' Nada) = First' Nada
  
firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool
type FstId = First' String -> Bool

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    x <- arbitrary
    frequency [ (3, return (First' (Only x)))
              , (1, return (First' Nada))]

-- main --
main :: IO ()
main = do
  quickCheck (monoidAssoc :: BullMappend)
  quickCheck (monoidLeftIdentity :: Bull -> Bool)
  quickCheck (monoidRightIdentity :: Bull -> Bool)

  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)

