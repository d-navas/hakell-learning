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
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BC
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.URI (URI, parseURI)
import qualified System.Random as SR
import Web.Scotty
 
alphaNum :: String
alphaNum = ['A'..'Z'] ++ ['0'..'9']
 
randomElement :: String -> IO Char
randomElement xs = do
  let maxIndex :: Int
      maxIndex = length xs - 1
  -- Right of arrow if IO Int, so randomDigit is Int
  randomDigit <- SR.randomRIO (0, maxIndex) :: IO Int
  return (xs !! randomDigit)
 
shortyGen :: IO [Char]
shortyGen = replicateM 7 (randomElement alphaNum)
 
saveURI :: R.Connection -> BC.ByteString -> BC.ByteString -> IO (Either R.Reply R.Status)
saveURI conn shortURI uri = R.runRedis conn $ R.set shortURI uri
 
getURI :: R.Connection -> BC.ByteString -> IO (Either R.Reply (Maybe BC.ByteString)
getURI conn shortURI = R.runRedis conn $ R.get shortURI
 
linkShorty :: String -> String
linkShorty shorty =
  concat [ "<a href=\""
         , shorty
         , "\">Copy and paste your short URL</a>"]
 
shortyCreated :: Show a => a -> String -> TL.Text
shortyCreated resp shawty =
  TL.concat [ TL.pack (show resp)
            , " shorty is: "
            , TL.pack (linkShorty shawty)]
 
shortyAintUri :: TL.Text -> TL.Text
shortyAintUri uri =
  TL.concat [ uri
            , " wasn't a url, did you forget http://?"]
 
shortyFound :: TL.Text -> TL.Text
shortyFound tbs = TL.concat ["<a href=\"", tbs, "\">", "</a>"]
 
app :: R.Connection -> ScottyM ()
app rConn = do
  get "/" $ do
    uri <- param "uri"
 
param :: Parsable a => Data.Text.Internal.Lazy.Text -> ActionM a
param =
  get "/user/:name/view" $ do
    let parsedUri :: Maybe URI
        parsedUri = parseURI (TL.unpack uri)
    case parsedUri of
      Just_ -> do
        shawty <- liftIO shortyGen
        let shorty = BC.pack shawty
            uri'= encodeUtf8 (TL.toStrict uri)
        resp <- liftIO (saveURI rConn shorty uri')
        html (shortyCreated resp shawty)
      Nothing -> Text (shortyAintUri uri)
  get "/:short" $ do
    short <- param "short"
    uri <- liftIO (getURI rConn short)
    case uri of
      Left reply -> text (TL.pack (show reply))
      Right mbBS -> case mbBS of
        Nothing -> text "uri not found"
        Just bs -> html (shortyFound tbs)
          where tbs :: TL.Text
                tbs = TL.fromStrict (decodeUtf8 bs)
  
main :: IO ()
main = do
  rConn <- R.connect R.defaulltConnectInfo
  scotty 3000 (app rConn) 
