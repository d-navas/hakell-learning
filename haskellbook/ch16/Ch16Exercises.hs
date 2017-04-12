{-# LANGUAGE FlexibleInstances #-}

module Ch16Exercises where

import GHC.Arr

{-- Determine if a valid Functor can be written for the datatype provided --}

-- 1.
-- A: can't write Functor cause Bool is kind * and Functor needs * -> *
data Bool = False | True 

-- 2.
-- A: can write Functor for datatype since it's kind * -> *
data BoolAndSomethingElse a = False' a | True' a

-- 3.
-- A: can write Functor for datatype since it's kind * -> *
data BoolAndMaybeSomethingElse a = Falsish | Truish a

-- 4. (use the kinds to guide you on this one)
-- A: can write Functor for datatype since it's kind * -> *
newtype Mu f = InF { outF :: f (Mu f) }

-- 5. (use the kinds to guide you on this one)
-- A: can't write Functor cause Bool is kind * and Functor needs * -> *
data D = D (Array Word Word) Int Int

{-- Rearrange the arguments to the type constructor of the datatype so the
   Functor instance works --}

-- 1.
data Sum b a = First a | Second b

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b

-- 2.
data Company a c b = DeepBlue a c | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- 3.
data More b a = L a b a | R b a b

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

{-- Write Functor instances for the following
   datatypes. --}

-- 1.
data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap f (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

-- 2.
newtype K a b = K a

instance Functor (K a) where
  fmap _ (K a) = K a

-- 3.
newtype Flip f a b = Flip (f b a)

newtype K' a b = K' a

instance Functor (Flip K' a) where
  fmap = undefined




main :: IO ()
main = putStrLn "hello"
