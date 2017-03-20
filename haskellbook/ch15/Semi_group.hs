module Semi_group where

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 15.13: Semigroup
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data NonEmpty' a = a :| [a] deriving (Eq, Ord, Show)

newtype NonEmpty'' a = NonEmpty'' (a, [a]) deriving (Eq, Ord, Show)


