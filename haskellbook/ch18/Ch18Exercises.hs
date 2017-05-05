{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Ch18Exercises where

import Control.Monad
import Control.Applicative
import Data.Monoid

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 18.7: Chapter Exercises
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
-- ~~~~~~ 1.
data Nope a = NopeDotJpg
 
instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg
 
instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg
 
instance Monad Nope where
  return = pure
  NopeDotJpg >>= _ = NopeDotJpg
 
-- ~~~~~~ 2.
data Either' b a = Left' a | Right' b
 
instance Functor (Either' b) where
  fmap f (Left' a) = Left' (f a)
  fmap _ (Right' b) = Right' b
 
instance Monoid b => Applicative (Either' b) where
  pure _ = Right' mempty
  Right' b <*> _ = Right' b
  _ <*> Right' b = Right' b
  Left' f <*> Left' a =  Left' (f a)

instance Monoid b => Monad (Either' b) where
  return = pure
  Right' b >>= _ = Right' b
  Left' a >>= f = f a

-- 3.
newtype Identity a = Identity a deriving (Eq, Ord, Show)
 
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
 
instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)
 
instance Monad Identity where
  return = pure
  Identity a >>= f = f a
 
-- 4.
data List a = Nil | Cons a (List a)
 
instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure a = Cons a Nil
  (Cons f Nil) <*> (Cons a Nil) = Cons (f a) Nil
  Cons f fs <*> Cons a as = Cons (f a) (fs <*> as)
  _ <*> Nil = Nil
  Nil <*> _ = Nil
 
instance Monad List where
  return = pure
  Nil >>= _ =  Nil
  as >>= f = join $ fmap f as
 
-- 5: Write the following functions using the methods provided by Monad and Functor.
-- Using identity and composition is fine but it has to typecheck with types provided.
 
-- A:
j :: Monad m => m (m a) -> m a
j = join
 
-- B:
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap
 
-- C:
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftA2
 
-- D:
a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)
 
-- E: You'll need recursion for this one
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = do
  x' <- f x
  fmap ((:) x') (meh xs f)
 
-- F: Hint: reuse "meh"
flipType :: Monad m => [m a] -> m [a]
flipType = flip meh id

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 19.1 Applied Structure. Code Samples
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 19.2 Monoid
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
 
main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    html (mconcat [ "<h1>Scotty, ", beam, " me up!"])
 
-- Concatenating connection parameters
runDb :: SqlPersist (ResourceT IO) a -> IO a
runDb query = do
  let connStr =
      foldr (\(k, v) t ->
             t <> (encodeUtf8 $
             k <> "=" <> v <> " "))
      "" params
  runResourceT . withPostgresqlConn connStr
    $ runSqlConn query
 
-- Concatenating key configurations
import XMonad
import Xmonad.Actions.Volume
import Data.Map.Lazy (fromList)
import Data.Monoid (mappend)
 
main = do
  xmonad def { keys =
    \c -> fromList [
      ((0, xK_F7),
       lowerVolume 4 >> return ()),
      ((0, xK_F8),
       raiseVolume 4 >> return ())
    ] `mappend` keys defaultConfig c
  }
 
-- the type of keys is a function
keys :: !(XConfig Layout -> Map (ButtonMask, KeySym) (X ()))
  
-- mappend = <> 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 19.3 Functors
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
import Data.Time.Clock
 
offsetCurrentTime :: NominalDiffTime -> IO UTCTime
offsetCurrentTime offset =
  fmap (addUTCTime (offset * 24 * 3600)) $
    getCurrentTime
   
-- 1. NominalDiffTime is a newtype of Pico and has a Num instance,
-- that's why the arithmetic works.
addUTCTime :: NominalDiffTime -> UTCTime -> UTCTime
 
-- 2.
getCurrentTime :: IO UTCTime
 
-- 3. fmap's type got specialized
fmap :: (UTCTime -> UTCTime) -> IO UTCTime -> IO UTCTime
 
  
import Data.Text (Text))
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4

textUuid :: IO Text
textUuid =
  fmap (T.pack . UUID.toString) UUIDv4.nextRandom
 
-- 1.
nextRandom :: IO UUID
 
-- 2.
toString :: UUID -> String
 
-- 3.
pack :: String -> Text
 
-- 4.
fmap :: (UUID -> Text) -> IO UUID -> IO Text
 
userAgent :: AppHandler (Maybe userAgent)
userAgent = (fmap . fmap) userAgent' getRequest
 
userAgent' :: Request -> Maybe userAgent
userAgent' req = getHeader "User-Agent" req
 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 19.4: Applicative
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
jsonSwitch :: Parser (a -> a)
jsonSwitch =
  infoOption $(hgRevStateTH jsonFormat)
 $ long "json"
  <> short 'J'
  <> help "Display JSON version information"
 
parseInfo :: ParserInfo (a -> a)
parseInfo = info (helper <*> verSwitch <* jsonSwitch) fullDesc
 
-- More parsing
parseJSON :: Value -> Parser a
(.:) :: FromJSON a => Object -> Text -> Parser a
 
instance FromJSON Payload where
  parseJSON (Object v) = Payload <$> v .: "from"
                                 <*> v .: "to"
                                 <*> v .: "subject"
                                 <*> v .: "body"
                                 <*> v .: "offset_seconds"
  parseJSON v          = typeMismatch "Payload" v
 
-- Same JSON but for CSV data
parseRecord :: Record -> Parser a
 
instance FromRecord Release where
  parseRecord v
    | V.length v = 5 = Release <$> v .! 0
                               <*> v .! 1
                               <*> v .! 2
                               <*> v .! 3
                               <*> v .! 4
    | otherwise = mzero
   
instance Deserializeable ShowInfoResp where
  parser = e2err =<< convertPairs . HM.fromList <$> parsePairs
    where
      parsePairs :: Parser [(Text, Text)]
      parsePairs = parsePair `sepBy` endOfLine
     
      parsePair = liftA2 (,) parseKey parseValue
      parseKey  = takeTill (== ':') <* kvSep
 
      kvSep     = string ": "
     
      parseValue = takeTill isEndOfLine
 
-- Something different
module Web.Shipping.Utils ((<||>)) where
 
import Control.Applicative (liftA2)
 
(<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<||>) = liftA2 (||)
 
 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 19.5: Monad
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
import Network.Socket
 
openSocket :: FilePath -> IO Socket
openSocket p = do
  sock <- socket AF_UNIX Stream defaultProtocol
  connect sock sockAddr
  return sock
  where sockAddr = SockAddUnix . encodeString $ p
 
-- binding over failure in initialization
main :: IO ()
main = do
  initAndFp <- runEitherT $ do
    fp <- tryHead NoConfig =<< getArgs
    initCfg <- load' fp
    return (initCfg, fp)
  either bail (uncurry boot) initAndFp
  where
    boot initCfg fp =
      void $ runMVC mempty
             oracleModel (core initCfg fp)
    bail NoConfig =
      errorExit "Please pass a config"
    bail (InvalidConfig e) =
      errorExit ("Invalid config" ++ show e)
    load' fp =
      hoistEither
      . fmapL InvalidConfig =<< lift (load fp)
 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 19.6: An end to end example
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~


-- main
main :: IO ()
main = putStrLn "Ch-18: The Monads"

