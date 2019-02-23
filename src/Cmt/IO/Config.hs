{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmt.IO.Config where

import ClassyPrelude

import Data.List        (nub)
import System.Directory (doesFileExist)

import Cmt.Parser.Config (config)
import Cmt.Types.Config

path :: FilePath
path = ".cmt"

checkFormat :: [Output] -> Config -> Either Text (Config, [Output])
checkFormat output (Config parts format) = do
    let partNames = nub $ partName <$> parts
    let formatNames = nub . catMaybes $ formatName <$> format
    if (length partNames + length output) /= length formatNames
        then Left "Parts and format do not match"
        else Right (Config parts format, output)

parse :: [Output] -> Text -> Either Text (Config, [Output])
parse output cfg = config cfg >>= checkFormat output

read :: IO Text
read = decodeUtf8 <$> readFile path

parseArgs :: [Text] -> [Output]
parseArgs []    = []
parseArgs [msg] = [("*", msg)]
parseArgs parts = [("*", unwords parts)]

load :: IO (Either Text (Config, [Output]))
load = do
    exists <- doesFileExist path
    output <- parseArgs <$> getArgs
    if exists
        then parse output <$> read
        else pure $ Left ".cmt file not found"
