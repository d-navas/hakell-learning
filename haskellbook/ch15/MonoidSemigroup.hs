module MonoidSemigroup where

import Data.Monoid

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

