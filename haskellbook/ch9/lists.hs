module Lists where

{- The List datatype -}
-- data [] a = [] | a : [a]

{- Pattern matching on lists -}

myWords :: String -> [String]
myWords x =
  case x of
    []       -> []
    (' ':xs) -> myWords xs
    _        -> takeWhile (/= ' ') x : myWords (dropWhile (/= ' ') x)



