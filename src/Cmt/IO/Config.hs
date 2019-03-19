{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmt.IO.Config
    ( load
    , readCfg
    , checkFormat
    ) where

import ClassyPrelude

import Data.List        (nub)
import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.FilePath  (takeDirectory, (</>))

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

checkParent :: FilePath -> IO (Maybe FilePath)
checkParent path = do
    let parent = takeDirectory path
    if parent /= path
        then checkDir parent
        else pure Nothing

checkDir :: FilePath -> IO (Maybe FilePath)
checkDir path = do
    let fp = path </> configFile
    exists <- doesFileExist fp
    if exists
        then pure $ Just fp
        else checkParent path

homeDir :: IO (Maybe FilePath)
homeDir = do
    home <- getHomeDirectory
    let fp = home </> configFile
    exists <- doesFileExist fp
    pure $
        if exists
            then Just fp
            else Nothing

findFile :: IO (Maybe FilePath)
findFile = do
    inDir <- checkDir =<< getCurrentDirectory
    case inDir of
        Just fp -> pure $ Just fp
        Nothing -> homeDir

load :: IO (Either Text Text)
load = do
    exists <- findFile
    case exists of
        Just path -> Right <$> read path
        Nothing   -> pure $ Left ".cmt file not found"

readCfg :: [Output] -> IO (Either Text (Config, [Output]))
readCfg output = do
    cfg <- load
    pure (parse output =<< cfg)
