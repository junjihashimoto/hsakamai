{-# LANGUAGE OverloadedStrings #-}

module Main where

import Akamai.NetStorage
import Options.Applicative
import Data.Yaml (decodeFileEither,encode)
import qualified Data.ByteString as B
import Data.Conduit.Binary
import Data.String (fromString)
import qualified System.Posix as S


data Command
  = Download String String
  | Upload String String
  | Ls String
  | Stat String
  | Delete String
  | Config
  deriving (Eq, Show)

downloadCmd :: Parser Command
downloadCmd = Download <$> (argument str (metavar "NetStoragePath")) <*> (argument str (metavar "Local"))

uploadCmd :: Parser Command
uploadCmd = Upload <$> (argument str (metavar "Local")) <*> (argument str (metavar "NetStoragePath"))

dirCmd :: Parser Command
dirCmd = Ls <$> (argument str (metavar "NetStoragePath"))

statCmd :: Parser Command
statCmd = Stat <$> (argument str (metavar "NetStoragePath"))

deleteCmd :: Parser Command
deleteCmd = Delete <$> (argument str (metavar "NetStoragePath"))

configCmd :: Parser Command
configCmd = pure Config

getFileSize :: String -> IO S.FileOffset
getFileSize path = do
  stat' <- S.getFileStatus path
  return (S.fileSize stat')

readAuth :: IO (Maybe Auth)
readAuth = do
  v <- decodeFileEither "netstorage.yml"
  case v of
    Right v' -> return $ Just v'
    Left err -> do
      print err
      return Nothing

run :: Command -> IO Bool
run (Download from to) = do
  mauth <- readAuth
  case mauth of
    Just auth -> do
      withSinkFile (fromString to) $ \sink -> do
        download auth (fromString from) $ \_ -> do
          sink
      return True
    Nothing  -> return False

run (Upload from to) = do
  mauth <- readAuth
  case mauth of
    Just auth -> do
      size <- getFileSize from
      _ <- withSourceFile (fromString from) $ \src -> do
        upload auth (fromString to) (fromIntegral size) src
      return True
    Nothing  -> return False

run (Ls path) = do
  mauth <- readAuth
  case mauth of
    Just auth -> do
      v <- dir auth (fromString path)
      case v of
        Right contents -> do
          B.putStr $ encode contents
          return True
        Left err -> do
          print err
          return False
    Nothing  -> return False

run (Stat path) = do
  mauth <- readAuth
  case mauth of
    Just auth -> do
      v <- stat auth (fromString path)
      case v of
        Right contents -> do
          B.putStr $ encode contents
          return True
        Left err -> do
          print err
          return False
    Nothing  -> return False

run (Delete path) = do
  mauth <- readAuth
  case mauth of
    Just auth -> do
      v <- delete auth (fromString path)
      print v
      return True
    Nothing  -> return False

run (Config) = do
  B.putStr $ encode $ Auth "host" "keyname" "key" 123 True
  return True

commands :: Parser Command
commands = subparser
           (  command "download"         (info downloadCmd    (progDesc "download"))
           <> command "upload"           (info uploadCmd      (progDesc "upload"))
           <> command "dir"              (info dirCmd         (progDesc "dir"))
           <> command "stat"             (info statCmd        (progDesc "stat"))
           <> command "delete"           (info deleteCmd      (progDesc "delete"))
           <> command "config"           (info configCmd      (progDesc "config"))
           )

opts :: ParserInfo Command
opts = info (commands <**> helper) idm

main :: IO ()
main = do
  cmd <- execParser opts
  _ <- run cmd
  return ()


