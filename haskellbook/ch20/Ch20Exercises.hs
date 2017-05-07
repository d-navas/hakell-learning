{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Ch20Exercises where


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 20.6: Chapter Exercises.
-- Write Foldable instances for the following datatypes.
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- 1.
newtype Constant a b = Constant a

instance Monoid a => Foldable (Constant a) where
  foldr _ z (Constant a) = z
  foldMap _ (Constant a) = mempty

-- 2.
data Two a b = Two a b

instance Foldable (Two a) where
  foldr f z (Two a b) = f b z
  foldMap f (Two a b) = f b

-- 3.
data Three a b c = Three a b c

instance Foldable (Three a b ) where
  foldr f z (Three a b c) = f c z
  foldMap f (Three a b c) = f c

-- 4
data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldr f z (Three' a b0 b1) = f b0 (f b1 z)
  foldMap f (Three' a b0 b1) = f b0 `mappend` f b1

-- 5
data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldr f z (Four' a b0 b1 b2) = f b0 (f b1 (f b2 z))
  foldMap f (Four' a b0 b1 b2) = f b0 `mappend` f b1 `mappend` f b2

-- 6. Write a filter function for Foldable types using foldMap
filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\a -> if f a then pure a else mempty)

-- main
main :: IO ()
main = putStrLn "...works..."
