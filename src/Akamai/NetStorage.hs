{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Akamai.NetStorage
  ( Auth(..)
  , Contents(..)
  , NetStoragePath
  , mkReq
  , download
  , dir
  , stat
  , delete
  , upload
  ) where


import Control.Exception.Base (SomeException)
import Crypto.Hash.Algorithms
import Crypto.MAC.HMAC
import Data.Aeson.TH
import Data.ByteArray.Encoding (convertToBase, Base (Base64))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower)
import Conduit (MonadUnliftIO)
import Data.Conduit
import Data.Int (Int64)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.UnixTime hiding (UnixTime)
import Data.Word (Word32)
import Foreign.C.Types (CTime)
import Network.HTTP.Client (responseBody)
import Network.HTTP.Simple
import System.Random (randomIO)
import Text.XML
import Text.XML.Cursor
import Control.Monad.IO.Class

type UnixTime = CTime
type UniqueId = Word32
type KeyName = Text
type NetStoragePath = ByteString
type AuthData = ByteString
type Key = Text
type Message = ByteString
type AuthSign = ByteString
type Action = ByteString
type SignString = ByteString

data Auth = Auth
  { authHostname :: Text
  , authKeyName :: KeyName
  , authKey :: Key
  , authCpCode :: Int64
  , authSsl :: Bool
  } deriving (Show,Read,Eq)

$(deriveJSON defaultOptions{fieldLabelModifier = (map toLower) . (drop 4)} ''Auth)

data Contents = File
  { fileName :: Text
  , fileSize :: Int64
  , fileMd5 :: Text
  , fileMtime :: Int64
  } | Dir
  { dirName :: Text
  } | Symlink
  { symlinkName :: Text
  } deriving (Show,Read,Eq)

$(deriveJSON defaultOptions ''Contents)

-- | authData
--
-- >>> authData 123 456 "key"
-- "5, 0.0.0.0, 0.0.0.0, 123, 456, key"
authData :: UnixTime -> UniqueId -> KeyName -> AuthData
authData ut uid kname = "5, 0.0.0.0, 0.0.0.0, " <> fromString (show ut) <> ", " <> fromString (show uid) <> ", " <> T.encodeUtf8 kname

-- | signString
--
-- >>> signString "path" "version=1&action=download"
-- "path\nx-akamai-acs-action:version=1&action=download\n"
signString :: NetStoragePath -> Action -> SignString
signString path action = path <> "\n" <> "x-akamai-acs-action:" <> action <> "\n"


-- | authSign
-- >>> authSign "abcdefghij" "5, 0.0.0.0, 0.0.0.0, 1280000000, 382644692, UploadAccountMedia/123456/files_baseball/sweep.m4a\nx-akamai-acs-action:version=1&action=upload&md5=0123456789abcdef0123456789abcdef&mtime=1260000000\n"
-- "yh1MXm/rv7RKZhfKlTuSUBV69Acph5IyOWCU0/nFjms="
authSign :: Key -> Message -> AuthSign
authSign key msg = convertToBase Base64 $ (hmac (T.encodeUtf8 key) msg :: HMAC SHA256)

auth' :: UnixTime -> UniqueId -> KeyName -> NetStoragePath -> Action -> Key -> (AuthData,AuthSign)
auth' ut uid kname path action key = (ad, sign)
  where
    ad = authData ut uid kname
    msg = ad <> signString path action
    sign = authSign key msg

authHeaders :: UnixTime -> UniqueId -> KeyName -> NetStoragePath -> Action -> Key -> RequestHeaders
authHeaders ut uid kname path action key =
  [
    ("X-Akamai-ACS-Action", action)
  , ("X-Akamai-ACS-Auth-Data", ad)
  , ("X-Akamai-ACS-Auth-Sign", sign)
  , ("Accept-Encoding", "identity")
  , ("User-Agent", "NetStorageKit-Haskell")
  ]
  where
    (ad, sign) = auth' ut uid kname path action key

-- | parseContents
--
-- >>> parseContents "<stat></stat>"
-- Right []
-- >>> parseContents "<stat><file type=\"file\" name=\"[CP Code]/File1.ext\" size=\"3\" md5=\"[HASH]\" mtime=\"1524068379\"/><file type=\"dir\"  name=\"[CP Code]/explicitdir1/\"/><file type=\"symlink\" name=\"[CP Code]/explicitdir2/link1\"/></stat>"
-- Right [File {fileName = "[CP Code]/File1.ext", fileSize = 3, fileMd5 = "[HASH]", fileMtime = 1524068379},Dir {dirName = "[CP Code]/explicitdir1/"},Symlink {symlinkName = "[CP Code]/explicitdir2/link1"}]
parseContents :: ByteString -> Either SomeException [Contents]
parseContents bin =
  case parseLBS def (BL.fromStrict bin) of
    Left err -> Left err
    Right doc ->
      let cursor = fromDocument doc
      in Right $ pure cursor
      >>= element "stat"
      >>= child
      >>= checkName ( == "file" )
      >>= \cur -> case (map (\a -> attribute a cur) ["type","name","size","md5","mtime"]) of
                    ["file"]:[name]:[size]:[md5]:[mtime]:_ -> [File name (read $ T.unpack size) md5 (read $ T.unpack mtime)]
                    ["dir"]:[name]:_ -> [Dir name]
                    ["symlink"]:[name]:_ -> [Symlink name]
                    _ -> []

mkReq :: MonadUnliftIO m => Auth -> ByteString -> ByteString -> Action -> m Request
mkReq auth method path action = do
  let path' = "/" <> BC.pack (show (authCpCode auth)) <> "/" <> path
  initReq <- liftIO $ parseRequest $ BC.unpack $ method <> " "
    <> (if authSsl auth then "https" else "http")
    <> "://" <> (T.encodeUtf8 (authHostname auth))
    <> path'
  ut <- liftIO $ (getUnixTime >>= return.utSeconds)
  uid <- liftIO (randomIO >>= return.(\v -> v `mod` 10000))
  return $ setRequestHeaders (authHeaders ut uid (authKeyName auth) path' action (authKey auth)) initReq

download :: MonadUnliftIO m => Auth -> NetStoragePath -> ((Response () -> ConduitM ByteString Void m a)) -> m a
download auth path fn = do
  req <- mkReq auth "GET" path "version=1&action=download"
  httpSink req fn

dir :: MonadUnliftIO m => Auth -> NetStoragePath -> m (Either SomeException [Contents])
dir auth path = do
  req <- mkReq auth "GET" path "version=1&action=dir&format=xml"
  res <- httpBS req
  return $ parseContents $ responseBody res

stat :: MonadUnliftIO m => Auth -> NetStoragePath -> m (Either SomeException [Contents])
stat auth path = do
  req <- mkReq auth "GET" path "version=1&action=stat&format=xml"
  res <- httpBS req
  return $ parseContents $ responseBody res

delete :: MonadUnliftIO m => Auth -> NetStoragePath -> m (Response ByteString)
delete auth path = do
  req <- mkReq auth "POST" path "version=1&action=delete"
  httpBS req

upload :: MonadUnliftIO m => Auth -> NetStoragePath -> Int64 -> (ConduitM () ByteString IO ()) -> m (Response ByteString)
upload auth path size fn = do
  initReq <- mkReq auth "PUT" path "version=1&action=upload&upload-type=binary"
  let req = setRequestBodySource size fn initReq
  httpBS req
