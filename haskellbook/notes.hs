module Mood where

data Mood = Woot | Blah deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah

-- Rational and Scientific are arbitrary precision types

-- ---------------------------
-- CHAPTER 4 EXERCISES
-- ---------------------------
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs a = if a < 0 then (-1) * a else a

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f a b = ((snd a, snd b), (fst a, fst b))

F :: xs -> Integer
F xs = 1 + (length xs)

-- type of Show
Show a => a -> String

-- type of (==)
Eq a => a -> a -> Bool

-- type of fst
(a, b) -> a

-- type of (+)
(+) :: Num a => a -> a -> a

-- Polymorphism is parametric (works for any type)
-- or constrained by the type of the arguments

-- We can give a concrete type to anything
x = 3 :: Integer

-- Num a: typeclass-constrained polymorphic type variable
(+) :: Num a => a -> a -> a

-- SECTIONING: partially applying infix operators (e.g. + / ^ -)
x = 5
y = (2^) -- waiting for number to power 2 to
y x -- 32

celebrate = (++ " woot!")
celebrate "naptime" -- "naptime woot!"

c = (`elem` [1..10])
c 9 -- True

-- You can check the types of things that aren't implemented yey
-- so long as you give GHCi an undefined to bind the signature to.
f :: a -> a -> a -> a; f = undefined

-- Polymorphic: Poly = many, Morph = form, -ic = made of

{- REMEMBER: Concrete types have even more flexibility in terms of computation.
This has to do with the additive nature of typeclasses. For
example, an Int is only an Int, but it can make use of the methods of
the Num and Integral typeclasses because it has instances of both -}

{- Parametricity is the property we get from having parametric polymorphism.
Parametricity means that the behavior of a function with respect to the
types of its (parametrically polymorphic) arguments is uniform. The
behavior can not change just because it was applied to an argument of a
different type. -}

-- Polymorphic constant
(-10) -- Num b => b

-- Ad-hoc Polymorphism = constrained polymorphism
