{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module TheFunctor where

import Test.QuickCheck
import Test.QuickCheck.Function

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 16.2: What is a Functor
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 16.3: There's a whole lot of fmap going round
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
data FixMePls a = FixMe | Pls a deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 16.5: Functor Laws
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~

-- Identity
-- fmap id == id

-- Composition
-- fmap (f . g) == fmap f . fmap g

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 16.6: The Good, the Bad, and the Ugly
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
data WhoCares a = ItDoesnt | Matter a | WhatThisIsCalled deriving (Eq, Show)

instance Functor WhoCares where
  fmap _ ItDoesnt = ItDoesnt
  fmap _ WhatThisIsCalled = WhatThisIsCalled
  fmap f (Matter a) = Matter (f a)

-- Law breaking instance. Structure is broken
-- instance Functor WhoCares where
--   fmap _ ItDoesnt = WhatThisIsCalled
--   fmap f WhatThisIsCalled = ItDoesnt
--   fmap f (Matter a) = Matter (f a)

-- Composable functor with extra param in data constructo
data CountingBad a = Heisenberg Int a deriving (Eq, Show)

instance Functor CountingBad where
  fmap f (Heisenberg n a) = Heisenberg n (f a)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 16.7: Commonly Used Functors
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~

-- The functors are stacked and that's a fact
lms = [Just "Ave", Nothing, Just "woohoo"]
replaceWithP = const 'p'
-- > replaceWithP lms --- R: 'p'
-- > fmap replaceWithP lms --- R: "ppp"
-- > (fmap . fmap) replaceWithP lms --- R: [Just 'p', Nothing, Just 'p']
-- > (fmap . fmap . fmap) replaceWithP lms --- R: [Just "ppp", Nothing, Just "pppppp"]

-- ~~~~~~~~~
-- One more round for the P-Funkshun
-- ~~~~~~~~~
replaceWithP' :: b -> Char
replaceWithP' = const 'p'

lms' :: [Maybe String]
lms' = [Just "Ave", Nothing, Just "woohoo"]

-- making the argument more specific
replaceWithP'' :: [Maybe [Char]] -> Char
replaceWithP'' = replaceWithP'

-- Prelude> :t fmap replaceWithP'
-- fmap replaceWithP' :: Functor f => f a -> f Char

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP'

-- we can assert a more specific type
liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 16.8: Transforming the Unapplied Type Argument
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data Two a b = Two a b deriving (Eq, Show)

data Or a b = First a | Second b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Functor (Or a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 16.9: QuickChecking Functor Instances
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = fmap g (fmap f x) == fmap (g . f) x

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 16.10: Exercises: Implement Functor instances
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- ~~~~~ 1:
newtype Identity a = Identity a deriving (Eq, Show)
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

-- ~~~~~ 2:
data Pair a = Pair a a deriving (Eq, Show)
instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

-- ~~~~~ 3:
-- Was already done by the book earlier

-- ~~~~~ 4:
data Three a b c = Three a b c deriving (Eq, Show)
instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

-- ~~~~~ 5:
data Three' a b = Three' a b b deriving (Eq, Show)
instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

-- ~~~~~ 6:
data Four a b c d = Four a b c d deriving (Eq, Show)
instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

-- ~~~~~ 7:
data Four' a b = Four' a a a b deriving (Eq, Show)
instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

-- ~~~~~ 8:
data Trivial = Trivial --can't be implemented since it's kind * and Functor takes * -> *

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 16.11: Ignoring Possibilities 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
incIfJust :: Num a => Maybe a -> Maybe a
incIfJust (Just n) = Just $ n + 1
incIfJust Nothing = Nothing

showIfJust :: Show a => Maybe a -> Maybe String
showIfJust (Just s) = Just $ show s
showIfJust Nothing = Nothing

-- using fmap
incMaybe :: Num a => Maybe a -> Maybe a
incMaybe m = fmap (+1) m

showMaybe :: Show a => Maybe a -> Maybe String
showMaybe s = fmap show s

-- point free
incMaybe' :: Num a => Maybe a -> Maybe a
incMaybe' = fmap (+1)

showMaybe' :: Show a => Maybe a -> Maybe String
showMaybe' = fmap show

-- generic functions (work with any datatype that implements Functor)
liftedInc :: (Functor f, Num b) => f b -> f b
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show

incEither' :: Num a => Either e a -> Either e a
incEither' = fmap (+1)

showEither' :: Show a => Either e a -> Either e String
showEither' = fmap show

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 16.12: A Somewhat Surprising Functor
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant m) where
  fmap _ (Constant v) = Constant v

-- Prelude> const 2 (getConstant (Constant 3))
-- 2

-- Prelude> fmap (const 2) (Constant 3)
-- Constant {getConstant = 3}

-- Prelude> getConstant $ fmap (const 2) (Constant 3)
-- 3

-- Prelude> getConstant $ fmap (const "blah") (Constant 3)
-- 3

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 16.13: More Structure, More Functors
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
newtype Wrap f a = Wrap (f a) deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 16.14: IO Functor
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
getInt :: IO Int
getInt = fmap read getLine -- fmap lifts over IO
-- > fmap (+1) getInt
-- > 10
-- > 11

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 16.15: What if we want to do something different?
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- What if we wanted to transform ony the structure and leave the type argument alone?
-- {-# LANGUAGE RankNTypes #-} <- needed to quantify a below
type Nat f g = forall a . f a -> g a

-- This will work
maybeToList :: Nat Maybe []
maybeToList Nothing  = []
maybeToList (Just a) = [a]

-- this will not work, not allowed
degenerateMtl :: Nat Maybe []
degenerateMtl Nothing = []
-- degenerateMtl (Just a) = [a + 1] -- doesn't work

-- bad nat
type Nat' f g a = f a -> g a

-- works
maybeToList' :: Nat' Maybe [] a
maybeToList' Nothing = []
maybeToList' (Just a) = [a]

-- also works if we tell it 'a' is Num a => a
degenerateMtl' :: Num a => Nat' Maybe [] a
degenerateMtl' Nothing = []
degenerateMtl' (Just a) = [a + 1] -- not good practice since it's changing original 'a' value

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 16.16: Functors are Unique to a Datatype
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- In a hypotetical non-Haskell language the following might be possible
data Tuple a b = Tuple a b deriving (Eq, Show)

-- this is impossible in Haskell
instance Functor (Tuple ? b) where
  fmap f (Tuple a b) = Tuple (f a) b

-- Haskell version of above
data Tuple' a b = Tuple' a b deriving (Eq, Show)
newtype Flip f a b = Flip (f b a) deriving (Eq, Show) -- f functor

-- this works
instance Functor (Flip Tuple a) where
  fmap f (Flip (Tuple a b)) = Flip $ Tuple (f a) b

-- >> fmap (+1) (Flip (Tuple 1 "blah"))
-- >> Flip (Tuple 2 "blah")

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 16.17: Chapter Exercises
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


main :: IO ()
main = do
  print (replaceWithP'' lms)
  quickCheck $ \x -> functorIdentity (x :: [Int])
  quickCheck $ \x -> functorCompose (+1) (*2) (x :: [Int])
  quickCheck (functorCompose' :: IntFC)

