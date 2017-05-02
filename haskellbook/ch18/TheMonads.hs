{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module TheMonads where

import Control.Monad (join)

-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   (>>) :: m a -> m b -> m b
--   return :: a -> m a

-- Core Operations
-- join :: Monad m => m (m a) -> m a

-- Exercise: Write bind in terms of fmap and join
-- keep in mind this is (>>=) flipped
bind :: Monad m => (a -> m b) -> m a -> m b
bind f ma = join (fmap f ma)

-- ~~~~~~~~~~~~~~~~~~
-- What Monad is not
-- ~~~~~~~~~~~~~~~~~~

sequencing' :: IO ()
sequencing' =
  putStrLn "hello, " *> -- sequencing
  putStrLn "world"

bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls: "
  name <- getLine
  putStrLn ("y helo thar: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "name please: " >>
  getLine >>= \name -> putStrLn ("y helo thar: " ++ name)

twoBinds :: IO ()
twoBinds = do
  putStrLn "name pls: "
  name <- getLine
  putStrLn "age pls: "
  age <- getLine
  putStrLn ("y helo thar: "
            ++ name ++" who is: "
            ++ age ++ " years old.")
 
twoBinds' :: IO ()
twoBinds' =
  putStrLn "name please: " >>
  getLine >>= \name -> putStrLn "age pls: " >>
  getLine >>= \age -> putStrLn ("y helo thar: "
            ++ name ++" who is: "
            ++ age ++ " years old.")
 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 18.4 Examples of Monad use
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
-- Example of the List Monad in use
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x * x, x * x]
    else []
 
-- Example of the Maybe monad in use
data Cow = Cow { name::String, age::Int, weight::Int } deriving (Eq, Show)
 
noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str
 
noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing
 
-- if Cow's name is Bess, must be under 500
weightCheck :: Cow -> Maybe Cow
weightCheck c = let w = weight c
                    n = name c
                in  if n == "Bess" && w > 499
                    then Nothing
                    else Just c
 
-- Using case syntax
mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty ->
              weightCheck (Cow nammy agey weighty)
 
-- Stack up dem nested lambdas
mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' =
  noEmpty name' >>=
    \nammy -> noNegative age' >>=
    \agey -> noNegative weight' >>=
    \weighty -> weightCheck (Cow nammy agey weighty)

-- Do syntax isn't just for IO
mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
  nammy <- noEmpty name'
  agey <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)
 
-- ===== Using the Either Monad =====
data SoftwareShop = Shop { founded :: Int, programmers:: Int } deriving (Eq, Show)
 
data FoundedError = NegativeYears Int
                   | TooManyYears Int
                   | NegativeCoders Int
                   | TooManyCoders Int
                   | TooManyCodersForYears Int Int deriving (Eq, Show)
 
validateFounded :: Int -> Either FoundedError Int
validateFounded n
  | n < 0 = Left $ NegativeYears n
  | n > 500 = Left $ TooManyYears n
  | otherwise = Right n
 
validateCoders :: Int -> Either FoundedError Int
validateCoders n
  | n < 0 = Left $ NegativeCoders n
  | n > 500 = Left $ TooManyCoders n
  | otherwise = Right n
 
mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded <- validateFounded years
  coders' <- validateCoders coders
  if coders' > div founded 10
    then Left $ TooManyCodersForYears founded coders'
    else Right $ Shop founded coders'
 
-- Exercise: implement the Either's Monad instance
data Sum a b = First a | Second b deriving (Eq, Show)
 
instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)
 
instance Applicative (Sum a) where
  pure = Second
  _ <*> First a = First a
  First a <*> _ = First a
  Second f <*> Second a = Second (f a)
 
instance Monad (Sum a) where
  return = pure
  First a >>= _ = First a
  Second a >>= f = f a

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 18.5: Monad Laws
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~

main :: IO ()
main = putStrLn "Ch-18: The Monads"

