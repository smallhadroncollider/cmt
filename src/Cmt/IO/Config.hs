{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmt.IO.Config where

import ClassyPrelude

import Data.List (nub)

import Cmt.Parser.Config (config)
import Cmt.Types.Config

checkFormat :: Config -> Either Text Config
checkFormat (Config parts format) = do
    let partNames = nub $ partName <$> parts
    let formatNames = nub . catMaybes $ formatName <$> format
    if length partNames > length formatNames
        then Left "Not all parts are used"
        else Right $ Config parts format

parse :: Text -> Either Text Config
parse cfg = config cfg >>= checkFormat
