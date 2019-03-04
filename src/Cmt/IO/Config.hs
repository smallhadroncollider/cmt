{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmt.IO.Config where

import ClassyPrelude

import Data.List        (nub)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath  (takeDirectory)

import Cmt.Parser.Config (config)
import Cmt.Types.Config

configFile :: FilePath
configFile = ".cmt"

checkFormat :: [Output] -> Config -> Either Text (Config, [Output])
checkFormat output (Config parts format) = do
    let partNames = nub $ partName <$> parts
    let formatNames = nub . catMaybes $ formatName <$> format
    let result
            | isJust (find (== "*") formatNames) && null output = Left "Command line entry missing"
            | (length partNames + length output) /= length formatNames =
                Left "Parts and format do not match"
            | otherwise = Right (Config parts format, output)
    result

parse :: [Output] -> Text -> Either Text (Config, [Output])
parse output cfg = config cfg >>= checkFormat output

read :: FilePath -> IO Text
read path = decodeUtf8 <$> readFile path

parseArgs :: [Text] -> [Output]
parseArgs []    = []
parseArgs [msg] = [("*", msg)]
parseArgs parts = [("*", unwords parts)]

parentDir :: FilePath -> IO (Maybe FilePath)
parentDir path = do
    let parent = takeDirectory path
    if parent /= path
        then findFile parent
        else pure Nothing

findFile :: FilePath -> IO (Maybe FilePath)
findFile path = do
    let fp = path <> "/" <> configFile
    exists <- doesFileExist fp
    if exists
        then pure $ Just fp
        else parentDir path

load :: IO (Either Text (Config, [Output]))
load = do
    exists <- findFile =<< getCurrentDirectory
    case exists of
        Just path -> do
            output <- parseArgs <$> getArgs
            parse output <$> read path
        Nothing -> pure $ Left ".cmt file not found"
