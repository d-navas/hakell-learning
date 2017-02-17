-- Will they work?

max (length [1, 2, 3]) (length [8,9,10,11,12])
-- Works

compare (3 * 4) (3 * 5)
-- Works

compare "Julie" True
-- No same type

(5 + 3) > (3 + 6)
-- Works

{--
  2. Does the following typecheck? If not, why not?
    A: does not, Mood does not implement/derive Eq
    data Mood = Blah
    | Woot deriving Show
    settleDown x = if x == Woot
    then Blah
    else x

  3. If you were able to get settleDown to typecheck:
    a) What values are acceptable inputs to that function?
        A: Mood type inputs
    b) What will happen if you try to run settleDown 9? Why?
        A: Error, Woot does not implement Eq
    c) What will happen if you try to run Blah > Woot? Why?
        A: Error, Mood does not implement Ord
--}

---- Hint: use some arithmetic operation to
---- combine values of type 'b'. Pick one.
arith :: Num b => (a -> b) -> Integer -> a -> b
arith f c a = (f a) + (fromInteger c)

-- IMPORTANT:
-- Instances are unique for a given combination oftypeclass and type



