{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Akamai.Edgegrid
  ( Auth(..)
  , mkReq
  , mkJsonReq
  ) where

import Crypto.Hash.Algorithms
import Crypto.MAC.HMAC
import Crypto.Hash
import Data.UUID.V4 (nextRandom)
import Data.UUID (toASCIIBytes)
import Data.CaseInsensitive (original)

import Data.Aeson
import Data.Aeson.TH
import Data.ByteArray.Encoding (convertToBase, Base (Base64))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.UnixTime hiding (UnixTime)
import Network.HTTP.Client.Conduit (RequestBody(..))
import Network.HTTP.Simple
import Network.HTTP.Types (hAuthorization, HeaderName)

type Message = ByteString
type AuthSign = ByteString
type Nonce = ByteString
type Method = ByteString
type PathWithQuery = ByteString
type CanonicalizedRequestHeaders = [(HeaderName,ByteString)]
type Body = ByteString

data Auth = Auth
  { authHostname :: Text
  , authAccessToken :: Text
  , authClientToken :: Text
  , authClientSecret :: Text
  } deriving (Show,Read,Eq)

$(deriveJSON defaultOptions{fieldLabelModifier = (map toLower) . (drop 4)} ''Auth)

-- | dataToSign
--
-- >>> base_url = "akaa-baseurl-xxxxxxxxxxx-xxxxxxxxxxxxx.luna.akamaiapis.net"
-- >>> access_token = "akab-access-token-xxx-xxxxxxxxxxxxxxxx"
-- >>> client_token = "akab-client-token-xxx-xxxxxxxxxxxxxxxx"
-- >>> client_secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx="
-- >>> nonce = "nonce-xx-xxxx-xxxx-xxxx-xxxxxxxxxxxx"
-- >>> timestamp = "20140321T19:34:21+0000"
-- >>> dataToSign (Auth base_url access_token client_token client_secret) "GET" "/" [] "" "20140321T19:34:21+0000" "nonce-xx-xxxx-xxxx-xxxx-xxxxxxxxxxxx"
-- "GET\thttps\takaa-baseurl-xxxxxxxxxxx-xxxxxxxxxxxxx.luna.akamaiapis.net\t/\t\t\tEG1-HMAC-SHA256 client_token=akab-client-token-xxx-xxxxxxxxxxxxxxxx;access_token=akab-access-token-xxx-xxxxxxxxxxxxxxxx;timestamp=20140321T19:34:21+0000;nonce=nonce-xx-xxxx-xxxx-xxxx-xxxxxxxxxxxx;"

-- "EG1-HMAC-SHA256 client_token=akab-client-token-xxx-xxxxxxxxxxxxxxxx;access_token=akab-access-token-xxx-xxxxxxxxxxxxxxxx;timestamp=20140321T19:34:21+0000;nonce=nonce-xx-xxxx-xxxx-xxxx-xxxxxxxxxxxx;signature=hXm4iCxtpN22m4cbZb4lVLW5rhX8Ca82vCFqXzSTPe4="
dataToSign :: Auth
           -> Method
           -> PathWithQuery
           -> CanonicalizedRequestHeaders
           -> Body
           -> ByteString
           -> Nonce
           -> ByteString
dataToSign auth method pathWithQuery creqHeaders body timeStamp nonce =
  method <> "\t" <>
  "https" <> "\t" <>
  T.encodeUtf8 (authHostname auth) <> "\t" <>
  pathWithQuery <> "\t" <>
  (B.intercalate "\t" $ map (\(k,v) -> original k <> ":" <> v) creqHeaders) <> "\t" <>
  (if body == "" then "" else contentHash body) <> "\t" <>
  "EG1-HMAC-SHA256 " <>
  "client_token=" <> T.encodeUtf8 (authClientToken auth) <> ";" <>
  "access_token=" <> T.encodeUtf8 (authAccessToken auth) <> ";" <>
  "timestamp=" <> timeStamp <> ";" <>
  "nonce=" <> nonce <> ";"

-- | authHeader
--
-- >>> authSign "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx=" "20140321T19:34:21+0000"
-- "znsRMDBRqTXGJ7Ojip3/h2FGPu3LuoMYWgv9PKEnE/o="
authSign :: ByteString -> Message -> AuthSign
authSign key msg = convertToBase Base64 $ (hmac key msg :: HMAC SHA256)

contentHash :: Message -> ByteString
contentHash msg = convertToBase Base64 $ (hash msg :: Digest SHA256)

-- | authHeader
--
-- >>> base_url = "akaa-baseurl-xxxxxxxxxxx-xxxxxxxxxxxxx.luna.akamaiapis.net"
-- >>> access_token = "akab-access-token-xxx-xxxxxxxxxxxxxxxx"
-- >>> client_token = "akab-client-token-xxx-xxxxxxxxxxxxxxxx"
-- >>> client_secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx="
-- >>> nonce = "nonce-xx-xxxx-xxxx-xxxx-xxxxxxxxxxxx"
-- >>> timestamp = "20140321T19:34:21+0000"
-- >>> authHeader (Auth base_url access_token client_token client_secret) "GET" "/" [] "" "20140321T19:34:21+0000" "nonce-xx-xxxx-xxxx-xxxx-xxxxxxxxxxxx"
-- "EG1-HMAC-SHA256 client_token=akab-client-token-xxx-xxxxxxxxxxxxxxxx;access_token=akab-access-token-xxx-xxxxxxxxxxxxxxxx;timestamp=20140321T19:34:21+0000;nonce=nonce-xx-xxxx-xxxx-xxxx-xxxxxxxxxxxx;signature=tL+y4hxyHxgWVD30X3pWnGKHcPzmrIF+LThiAOhMxYU="
-- >>> authHeader (Auth base_url access_token client_token client_secret) "GET" "/testapi/v1/t1?p1=1&p2=2" [] "" "20140321T19:34:21+0000" "nonce-xx-xxxx-xxxx-xxxx-xxxxxxxxxxxx"
-- "EG1-HMAC-SHA256 client_token=akab-client-token-xxx-xxxxxxxxxxxxxxxx;access_token=akab-access-token-xxx-xxxxxxxxxxxxxxxx;timestamp=20140321T19:34:21+0000;nonce=nonce-xx-xxxx-xxxx-xxxx-xxxxxxxxxxxx;signature=hKDH1UlnQySSHjvIcZpDMbQHihTQ0XyVAKZaApabdeA="
-- >>> authHeader (Auth base_url access_token client_token client_secret) "POST" "/testapi/v1/t3" [] "datadatadatadatadatadatadatadata" "20140321T19:34:21+0000" "nonce-xx-xxxx-xxxx-xxxx-xxxxxxxxxxxx"
-- "EG1-HMAC-SHA256 client_token=akab-client-token-xxx-xxxxxxxxxxxxxxxx;access_token=akab-access-token-xxx-xxxxxxxxxxxxxxxx;timestamp=20140321T19:34:21+0000;nonce=nonce-xx-xxxx-xxxx-xxxx-xxxxxxxxxxxx;signature=hXm4iCxtpN22m4cbZb4lVLW5rhX8Ca82vCFqXzSTPe4="
authHeader :: Auth
           -> Method
           -> PathWithQuery
           -> CanonicalizedRequestHeaders
           -> Body
           -> ByteString
           -> Nonce
           -> ByteString
authHeader auth method pathWithQuery creqHeaders body timeStamp nonce =
  "EG1-HMAC-SHA256 " <>
  "client_token=" <> T.encodeUtf8 (authClientToken auth) <> ";" <>
  "access_token=" <> T.encodeUtf8 (authAccessToken auth) <> ";" <>
  "timestamp=" <> timeStamp <> ";" <>
  "nonce=" <> nonce <> ";" <>
  "signature=" <> signature
  where
    signature = authSign signingKey (dataToSign auth method pathWithQuery creqHeaders body' timeStamp nonce)
    signingKey = authSign (T.encodeUtf8 (authClientSecret auth)) timeStamp
    body' = B.take 131072 body

mkReq :: Auth
      -> Method
      -> PathWithQuery
      -> CanonicalizedRequestHeaders
      -> Body
      -> IO Request
mkReq auth method pathWithQuery creqHeaders body = do
  initReq <- parseRequest $ BC.unpack $ method <> " "
    <> "https"
    <> "://" <> (T.encodeUtf8 (authHostname auth))
    <> pathWithQuery
  timeStamp <- (getUnixTime >>= return.(formatUnixTimeGMT "%Y%m%dT%H:%M:%S%z"))
  uuid <- (nextRandom >>= return.toASCIIBytes)
  return $
    setRequestBody (RequestBodyBS body) $
    setRequestHeaders ((hAuthorization,authHeader auth method pathWithQuery creqHeaders body timeStamp uuid):creqHeaders) initReq

mkJsonReq :: ToJSON a
          => Auth
          -> Method
          -> PathWithQuery
          -> CanonicalizedRequestHeaders
          -> a
          -> IO Request
mkJsonReq auth method pathWithQuery creqHeaders body = do
  initReq <- parseRequest $ BC.unpack $ method <> " "
    <> "https"
    <> "://" <> (T.encodeUtf8 (authHostname auth))
    <> pathWithQuery
  timeStamp <- (getUnixTime >>= return.(formatUnixTimeGMT "%Y%m%dT%H:%M:%S%z"))
  uuid <- (nextRandom >>= return.toASCIIBytes)
  return $
    setRequestHeader "Content-Type" ["application/json"] $
    setRequestBodyJSON body $
    setRequestHeaders ((hAuthorization,authHeader auth method pathWithQuery creqHeaders (BL.toStrict (encode body)) timeStamp uuid):creqHeaders) initReq

