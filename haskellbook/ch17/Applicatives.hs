module Applicatives where

import Data.List (elemIndex)
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers -- provided by external library
import Test.QuickCheck.Classes -- provided by external library

-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- 17.2: Defining Applicative
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- So, every type that can have an Applicative instance must also have a Functor instance
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

-- Core Control.Applicative functions
liftA :: Applicative f => (a -> b)
          -> f a
          -> f b

liftA2 :: Applicative f => (a -> b -> c)
          -> f a
          -> f b
          -> f c

liftA3 :: Applicative f => (a -> b -> c -> d)
          -> f a
          -> f b
          -> f c
          -> f d

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 17.3: Functor vs Applicative
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fmap (+1) [1..3] is equivalent to pure (+1) <*> [1..3]

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 17.4: Applicative Functors are monoidal functors
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mappend :: Monoid a => a -> a -> a

{- Show me the monoids -}

{- Tuple Monoid nad Applicative side by side -}
instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)
  (a, b) `mappend` (a', b') = (a `mappend` a', b `mappend` b')

instance Monoid a => Applicative ((,) a) where
  pure x = (mempty, x)
  (u, f) <*> (v, x) = (u `mappend` v, f x)

{- Maybe Monoid and Applicative -}
instance Monoid a => Monoid a (Maybe a) where
  mempty = Nothing
  mappend m Nothing = m
  mappend Nothing m = m
  mappend (Just a) (Just a') = Just (a `mappend` a')

instance Applicative Maybe where -- implicit Monoid (see it?)
  pure = Just
  Nothing <*> _ = Nothing
  Just f <*> Just a = Just (f a)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 17.5: Applicative in Use
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
f x = lookup x [(3, "hello"), (4, "julie"), (5, "kbai")]
g y = lookup y [(7, "sup?"), (8, "chris"), (9, "aloha")]
h z = lookup z [(2, 3), (5, 6), (7, 8)]
m x = lookup x [(4, 10), (8, 13), (1, 9001)]

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

-- Exercise: Identity Instance
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

-- Exercise: Constant Instance
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant { getConstant = mempty }
  (Constant a) <*> (Constant a') = Constant (a `mappend` a')

-- Using the Maybe Applicative
validateLength :: Int -> String -> Maybe String
validateLength maxLen s = if length s > maxLen then Nothing else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = Name <$> validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress s = Address <$> validateLength 100 s

data Person = Person Name Address deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a = Person <$> mkName n <*> mkAddress a

-- Exersise: Fixer Upper
const <$> Just "Hello" <*> pure "World"
(,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 17.6: Applicative Laws
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- 1. Identity:
  pure id <*> v = v

  -- Example (all equal):
  id [1..5]
  fmap id [1..5]
  pure id <*> [1..5]

-- 2. Composition:
  pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

-- 3. Homomorphism:
  pure f <*> pure x = pure (f x)

-- e.g.:
  pure (+1) <*> pure 1 = pure ((+1) 1)

-- 4. Interchange:
  u <*> pure y = pure ($ y) <*> u

  -- (u: function embedded in some strcuture)
  -- e.g.
  Just (+2) <*> pure 2

  -- According to the law, following are equal:
  Just (+2) <*> pure 2 == pure ($ 2) <*> Just (+2)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 17.7: QuickChecking the Applicative Laws
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [ (1, return Fools)
                        , (1, return Twoo)]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

instance EqProp Bull where (=-=) = eq

main :: IO ()
main = quickBatch (monoid Twoo)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 17.8: ZipList Monoid
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~













