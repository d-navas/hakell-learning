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
  fmap f (Flip (K' a)) = Flip $ K' (f a)

-- 4.
newtype EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

-- 5.
newtype LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

-- 6.
data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (f <$> fa) (f <$> ga)

-- 7.
data IgnoreOne f g a b = IngnoringSomething (f a) (g b)

instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IngnoringSomething fa gb) = IngnoringSomething fa (f <$> gb)

-- 8.
data Notorious g o a t = Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (f <$> gt)

-- 9.
data List a = Nil | Cons a (List a)

instance Functor List where
  fmap _ Nil= Nil
  fmap f (Cons a la) = Cons (f a) (f <$> la)

-- 10.
data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats fa fb fc) = MoreGoats (f <$> fa) (f <$> fb) (f <$> fc)

-- 11.
data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print t a) = Print t (f a)
  fmap f (Read h) = Read (f . h)


main :: IO ()
main = putStrLn "Ch16 Exercises"
