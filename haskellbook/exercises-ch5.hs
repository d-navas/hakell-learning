module Sing where

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> String
sndString x = x ++ " over the rainbow"

sing = if (x > y) then fstString x else sndString y
  where
    x = "Singing"
    y = "Somewhere"

-- module Arith3Broken where

main :: IO ()
main = do
  print (1 + 2)
  putStrLn "10"
  print (negate (-1))
  print ((+) 0 blah)
    where blah = negate 1
