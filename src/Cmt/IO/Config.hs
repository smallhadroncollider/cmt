{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Cmt.IO.Config
    ( load
    , findFile
    , readCfg
    , checkFormat
    ) where

import ClassyPrelude

import Data.List        (nub)
import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.FilePath  (takeDirectory, (</>))

import Cmt.Parser.Config (config)
import Cmt.Types.Config
import Cmt.Utility       ((<?>))

configFile :: FilePath
configFile = ".cmt"

checkFormat :: Outputs -> Config -> Either Text (Config, Outputs)
checkFormat output (Config parts format) = do
    let partNames = nub $ partName <$> parts
    let formatNames = nub . catMaybes $ formatName <$> format
    let result
            | isJust (find (== "*") formatNames) && null output = Left "Command line entry missing"
            | (length partNames + length output) /= length formatNames =
                Left "Parts and format do not match"
            | otherwise = Right (Config parts format, output)
    result

parse :: Outputs -> Text -> Either Text (Config, Outputs)
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

xdgCfg :: IO (Maybe FilePath)
xdgCfg = do
    fp <- (</> ".config" </> "cmt" </> configFile) <$> getHomeDirectory
    exists <- doesFileExist fp
    pure $ bool Nothing (Just fp) exists

homeCfg :: IO (Maybe FilePath)
homeCfg = do
    fp <- (</> configFile) <$> getHomeDirectory
    exists <- doesFileExist fp
    pure $ bool Nothing (Just fp) exists

globalCfg :: IO (Maybe FilePath)
globalCfg = do
    xdg <- xdgCfg
    home <- homeCfg
    pure $ xdg <?> home

findFile :: IO (Maybe FilePath)
findFile = do
    inDir <- checkDir =<< getCurrentDirectory
    case inDir of
        Just fp -> pure $ Just fp
        Nothing -> globalCfg

load :: IO (Either Text Text)
load = do
    exists <- findFile
    case exists of
        Just path -> Right <$> read path
        Nothing   -> pure $ Left ".cmt file not found"

readCfg :: Outputs -> IO (Either Text (Config, Outputs))
readCfg output = do
    cfg <- load
    pure (parse output =<< cfg)
