module ExeHeavyLifting where

-- 1:
a = (+1) <$> read "[1]" :: [Int]

-- 2:
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3:
c = (*2) . (\x -> x - 2)

-- 4:
d = ((return '1' ++) . show) . (\x -> [x, 1..3])

-- 5:
-- No clue. Very weird problem.

main :: IO ()
main = return ()
