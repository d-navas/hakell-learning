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
-- fmap id = id

-- Composition
-- fmap (f . g) = fmap f . fmap g

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

-- ~~~~~~~~~~
-- Main
-- ~~~~~~~~~~
main :: IO ()
main = return ()
