module  ChapterExer where

-- 1
-- tensDigit :: Integral a => a -> a
-- tensDigit x = d
--   where xLast = x `div` 10
--         d     = xLast `mod` 10

-- Rewrite using divMod
tensDigit :: Integral a => a -> a -- same type as original
tensDigit x = snd $ divMod x 10

-- 2
foldBool :: a -> a -> Bool -> a
foldBool x y True  = x
foldBool x y False = y

-- 3
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (b, c)
  where b = f a

-- 4
roundTrip :: (Show a, Read b) => a -> b
roundTrip a = read (show a)

main :: IO ()
main = do
  print (roundTrip 4::Int)

