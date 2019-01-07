{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Akamai.Purge
  ( Network(..)
  , invalidateByUrl
  , invalidateByCpCode
  , invalidateByCacheTag
  , deleteByUrl
  , deleteByCpCode
  , deleteByCacheTag
  ) where

import Akamai.Edgegrid
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Network.HTTP.Simple

data Network = Production | Staging deriving (Show,Read,Eq)

data PurgeResponse = PurgeResponse
  { httpStatus :: Int
  , estimatedSeconds :: Int
  , purgeId :: Text
  , supportId :: Text
  , detail :: Text
  } deriving (Show,Read,Eq)

$(deriveJSON defaultOptions ''PurgeResponse)

invalidateByUrl :: Auth -> Network -> [Text] -> IO (Response PurgeResponse)
invalidateByUrl auth network urls = do
  let env = if network == Production then "production" else "staging"
  req <- mkJsonReq auth "POST" ("/ccu/v3/invalidate/url/" <> env) [] $
    object ["objects" .= urls]
  httpJSON req


invalidateByCpCode :: Auth -> Network -> [Int] -> IO (Response PurgeResponse)
invalidateByCpCode auth network cpcodes = do
  let env = if network == Production then "production" else "staging"
  req <- mkJsonReq auth "POST" ("/ccu/v3/invalidate/cpcode/" <> env) [] $
    object ["objects" .= (map (\v -> Number (fromIntegral v)) cpcodes)]
  httpJSON req

invalidateByCacheTag ::  Auth -> Network -> [Text] -> IO (Response PurgeResponse)
invalidateByCacheTag auth network tags = do
  let env = if network == Production then "production" else "staging"
  req <- mkJsonReq auth "POST" ("/ccu/v3/invalidate/tag/" <> env) [] $
    object ["objects" .= tags]
  httpJSON req

deleteByUrl :: Auth -> Network -> [Text] -> IO (Response PurgeResponse)
deleteByUrl auth network urls = do
  let env = if network == Production then "production" else "staging"
  req <- mkJsonReq auth "POST" ("/ccu/v3/delete/url/" <> env) [] $
    object ["objects" .= urls]
  httpJSON req

deleteByCpCode :: Auth -> Network -> [Int] -> IO (Response PurgeResponse)
deleteByCpCode auth network cpcodes = do
  let env = if network == Production then "production" else "staging"
  req <- mkJsonReq auth "POST" ("/ccu/v3/delete/cpcode/" <> env) [] $
    object ["objects" .= (map (\v -> Number (fromIntegral v)) cpcodes)]
  httpJSON req

deleteByCacheTag :: Auth -> Network -> [Text] -> IO (Response PurgeResponse)
deleteByCacheTag auth network tags = do
  let env = if network == Production then "production" else "staging"
  req <- mkJsonReq auth "POST" ("/ccu/v3/delete/tag/" <> env) [] $
    object ["objects" .= tags]
  httpJSON req
