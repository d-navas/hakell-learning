module TheFunctor where

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


-- Main
main :: IO ()
main =
  print (replaceWithP'' lms)
