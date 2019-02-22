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

checkFormat :: Config -> Either Text Config
checkFormat (Config parts format) = do
    let partNames = nub $ partName <$> parts
    let formatNames = nub . catMaybes $ formatName <$> format
    if length partNames > length formatNames
        then Left "Not all parts are used"
        else Right $ Config parts format

parse :: Text -> Either Text Config
parse cfg = config cfg >>= checkFormat

read :: IO Text
read = decodeUtf8 <$> readFile path

load :: IO (Either Text Config)
load = do
    exists <- doesFileExist path
    if exists
        then parse <$> read
        else pure $ Left ".cmt file not found"
