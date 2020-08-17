{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Cmt.Parser.Config
    ( config
    , predefined
    ) where

import ClassyPrelude

import Data.Attoparsec.Text

import Cmt.Parser.Config.Branches   (branchesP)
import Cmt.Parser.Config.Format     (formatP)
import Cmt.Parser.Config.Parts      (partsP)
import Cmt.Parser.Config.PreDefined (predefinedPartsP)
import Cmt.Types.Config

configP :: Parser Config
configP = do
    branches <- branchesP
    parts <- partsP
    _ <- predefinedPartsP branches parts
    format <- formatP $ partName <$> parts
    _ <- endOfInput
    pure $ Config branches parts format

predefinedP :: Parser PreDefinedParts
predefinedP = do
    branches <- branchesP
    parts <- partsP
    pre <- predefinedPartsP branches parts
    _ <- formatP $ partName <$> parts
    _ <- endOfInput
    pure pre

-- run parser
config :: Text -> Either Text Config
config cfg =
    case parseOnly configP cfg of
        Right c -> Right c
        Left _ ->
            Left "Could not parse config. Check that your format doesn't contain any invalid parts."

predefined :: Text -> Either Text PreDefinedParts
predefined cfg =
    case parseOnly predefinedP cfg of
        Right c -> Right c
        Left _  -> Left "Could not parse config"
