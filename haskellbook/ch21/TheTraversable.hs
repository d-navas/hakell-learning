{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- {-# MINIMAL traverse' | sequenceA' #-}

module TheTraversable where

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 21.2: The Traversable Typeclass Definition
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
class (Functor t, Foldable t) => Traversable' t where
  traverse' :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse' f = sequenceA' . fmap f

  sequenceA' :: Applicative f => t (f a) -> f (t a)
  sequenceA' = traverse' id

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 21.3: sequenceA
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Flips layers of structure around

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 21.4: traverse
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
traverse f = sequenceA . fmap f
 
-- Similarities
fmap ::     (a ->   b) -> f a -> f b
(=<<) ::    (a -> m b) -> m a -> m b
traverse :: (a -> f b) -> t a -> f (t b)
 
-- mapM is just traverse
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
    -- constrast with
traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
-- ^ Generalizes the [] in mapM and the requirement is only Applicative
 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 21.5: So, What's traversable for?
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- In a literal sense, anytime you need to flip two type constructors
-- around, or map something and then flip them around, that's probably traversable.
sequenceA :: Applicative f =>    t (f a) -> f (t a)
traverse ::  Applicative f => (a -> f b) -> t a -> f (t b)
 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 21.6: Morse Code Revisited
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stringToMorse :: String -> Maybe [Morse]
stringToMorse s = sequence $ fmap charToMorse s
 
-- what we want to do:
stringToMorse :: string -> Maybe [Morse]
stringToMorse = traverse charToMorse
 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 21.7: Axing Tedious Code
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data Query = Query
data SomeObj = SomeObj
data IoOnlyObj = IoOnlyObj
data Err = Err
 
-- there's a decoder function that makes
-- some object from string
decodeFn :: String -> Either Err SomeObj
decodeFn = undefined
 
-- there's a query, that runs against DB and
-- returns array of strings
fetchFn :: Query -> IO [String]
fetchFn = undefined
 
-- there's some additional "context initializer",
-- that also has IO side-effects
makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined
 
-- before
pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  case sequence (map decodeFn a) of
    (Left err) -> return $ Left $ err
    (Right res) -> do
      a <- makeIoOnlyObj res
      return $ Right a
-- better version
pipelineFn' = (traverse makeIoOnlyObj. traverse decodeFn =<<) . fetchFn
 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 21.8: Do all the things
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- using HTTP client lib "wreq"
module HttpStuff where
 
import Data.ByteString.Lazy hiding (map)
import Network.Wreq
 
-- replace with other websites
-- if desired or needed
urls :: [String]
urls = [ "http://httpbin.com/ip"
       , "http://httpbin.org/bytes/5"]
 
mappingGet :: [IO (Response ByteString)]
mappingGet = map get urls
 
traversedUrls :: IO [Response ByteString]
traversedUrls = traverse get urls
 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 21.9: Traversable Instances
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Either
data Either a b = Left a | Right b deriving (Eq, Ord, Show)
 
instance Functor (Either a) where
  fmap _ (Left x) = Left x
  fmap f (Right y) = Right (f y)
 
instance Applicative (Either e) where
  pure = Right
  Left e <*> _ = Left e
  Right f <*> r = fmap f r
 
instance Foldable (Either a) where
  foldMap _ (Left _) = mempty
  foldMap f (Right y) = f y
  foldr _ z (Left _) = z
  foldr f z (Right y) = f y z
 
instance Traversable (Either a) where
  traverse _ (Left x) = pure (Left x)
  traverse f (Right y) = Right <$> f y

-- Tuple
instance Functor ((,) a) where
  fmap f (x, y) = (x, f y)

instance Monoid a => Applicative ((,) a) where
  pure x = (mempty, x)
  (u, f) <*> (v, x) = (u `mappend` v, f x)

instance Foldable ((,) a) where
  foldMap f (_, y) = f y
  foldr f z (_, y) = f y z

instance Traversable ((,) a) where
  traverse f (x, y) = (,) x <$> f y

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 21.10: Traversable Laws
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 1. Naturality
t . traverse f = traverse (t . f)

-- 2. Identity
traverse Identity = Identity

-- 3. Composition
traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse 

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 21.10: sequenceA Laws
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




-- main
main :: IO ()
main = putStrLn "...works..."

