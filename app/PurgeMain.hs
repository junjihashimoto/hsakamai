{-# LANGUAGE OverloadedStrings #-}

module Main where

import Akamai.Edgegrid
import Akamai.Purge
import Options.Applicative
import Data.Yaml (decodeFileEither,encode)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Network.HTTP.Client (responseBody)


data Command
  = InvalidateUrl Network String
  | InvalidateCpCode Network Int
  | InvalidateTag Network String
  | DeleteUrl Network String
  | DeleteCpCode Network Int
  | DeleteTag Network String
  | Config
  deriving (Eq, Show)

invalidateUrlCmd :: Parser Command
invalidateUrlCmd = InvalidateUrl <$> (argument auto (metavar "network")) <*> (argument str (metavar "url"))

invalidateCpCodeCmd :: Parser Command
invalidateCpCodeCmd = InvalidateCpCode <$> (argument auto (metavar "network")) <*> (argument auto (metavar "cpcode"))

invalidateTagCmd :: Parser Command
invalidateTagCmd = InvalidateTag <$> (argument auto (metavar "network")) <*> (argument str (metavar "tag"))

deleteUrlCmd :: Parser Command
deleteUrlCmd = DeleteUrl <$> (argument auto (metavar "network")) <*> (argument str (metavar "url"))

deleteCpCodeCmd :: Parser Command
deleteCpCodeCmd = DeleteCpCode <$> (argument auto (metavar "network")) <*> (argument auto (metavar "cpcode"))

deleteTagCmd :: Parser Command
deleteTagCmd = DeleteTag <$> (argument auto (metavar "network")) <*> (argument str (metavar "tag"))

configCmd :: Parser Command
configCmd = pure Config

readAuth :: IO (Maybe Auth)
readAuth = do
  v <- decodeFileEither "edgegrid.yml"
  case v of
    Right v' -> return $ Just v'
    Left err -> do
      print err
      return Nothing

run :: Command -> IO Bool
run cmd = do
  case cmd of
    Config -> do
      B.putStr $ encode $ Auth "host" "access-token" "client-token" "client-secret"
      return True
    InvalidateUrl network url -> run' invalidateByUrl network [T.pack url]
    InvalidateCpCode network cpcode -> run' invalidateByCpCode network [cpcode]
    InvalidateTag network tag -> run' invalidateByCacheTag network [T.pack tag]
    DeleteUrl network url -> run' deleteByUrl network [T.pack url]
    DeleteCpCode network cpcode -> run' deleteByCpCode network [cpcode]
    DeleteTag network tag -> run' deleteByCacheTag network [T.pack tag]
  where
    run' fn network arg = do
      mauth <- readAuth
      case mauth of
        Just auth -> do
          v <- fn auth network arg
          B.putStr $ encode $ responseBody v
          return True
        Nothing  -> return False


commands :: Parser Command
commands = subparser
           (  command "invalidate-url"    (info invalidateUrlCmd    (progDesc "invalidate-url"))
           <> command "invalidate-cpcode" (info invalidateCpCodeCmd (progDesc "invalidate-cpcode"))
           <> command "invalidate-tag"    (info invalidateTagCmd    (progDesc "invalidate-tag"))
           <> command "delete-url"        (info deleteUrlCmd        (progDesc "delete-url"))
           <> command "delete-cpcode"     (info deleteCpCodeCmd     (progDesc "delete-cpcode"))
           <> command "delete-tag"        (info deleteTagCmd        (progDesc "delete-tag"))
           <> command "config"            (info configCmd           (progDesc "config"))
           )

opts :: ParserInfo Command
opts = info (commands <**> helper) idm

main :: IO ()
main = do
  cmd <- execParser opts
  _ <- run cmd
  return ()


