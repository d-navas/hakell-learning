{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Ch20Exercises where


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 20.6: Chapter Exercises.
-- Write Foldable instances for the following datatypes.
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- 1.
newtype Constant a b = Constant a

-- 2.
data Two a b = Two a b

-- 3.
data Three a b c = Three a b c

-- 4
data Three' a b = Three' a b b

-- 5
data Four' a b = Four' a b b b





main :: IO ()
main = putStrLn "...works..."
