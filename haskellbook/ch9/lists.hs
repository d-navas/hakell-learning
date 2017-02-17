module Lists where

{- The List datatype -}
-- data [] a = [] | a : [a]

{- Pattern matching on lists -}

myWords :: String -> [String]
myWords "" = []
myWords x = (takeWhile (/= ' ') x) : myWords y
  where
    y = if x !! 0 == ' ' then tail x else (dropWhile (/= ' ') x)
