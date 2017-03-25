module TheFunctor where

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 16.2: What is a Functor
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 16.3: There's a whole lot of fmap going round
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 16.5: Functor Laws
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~

-- Identity
-- fmap id = id

-- Composition
-- fmap (f . g) = fmap f . fmap g

data FixMePls a = FixMe | Pls a deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)

main :: IO ()
main = return ()
