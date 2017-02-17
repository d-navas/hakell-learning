-- EXERCISES: Variety Pack

-- 1. Given the following:
k (x, y) = x
k1 = k ((4 - 1), 10)
k2 = k ("three", (1 + 2))
k3 = k (3, True)

-- a) what is the type of k?
k :: (x, y) -> x

-- b) what is the type of k2?
k2 :: Num t => t

-- c) Of k1, k2, k3, which will return the number 3 as teh result?
-- k1 and k3

-- ================================================
-- Fill in the definition of the following function
-- Remember: tuples have the same syntax for their
-- type constructors and their data constructors

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- CASE EXPRESSIONS
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
funcZ x =
  case x + 1 == 1 of
    True  -> "Awesome"
    False -> "wut"

-- Is palindrome
pal xs =
  case xs == reverse xs of
    True  -> "yes"
    False -> "no"

-- Greet if Cool
greetIfCool :: String -> IO ()
greetIfCool coolness =
  case cool of
    True  -> putStrLn "eyyy. What's shaking'?"
    False -> putStrLn "pshhhh"
  where cool = coolness == "downright frosty yo"

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Higher Order Functions
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x
  --or
flip f = \x y -> f y x

-- case expression
data Employee = Coder | Manager | Veep | CEO deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' = putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank :: Employee -> Employee -> IO ()
employeeRank e e' =
  case compare e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither is the boss"
    LT -> (flip reportBoss) e e'
