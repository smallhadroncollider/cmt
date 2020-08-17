{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}

module Cmt.Parser.Config.PreDefined
    ( predefinedPartsP
    ) where

import ClassyPrelude

import Control.Monad.Fail   (fail)
import Data.Attoparsec.Text

import Cmt.Parser.Attoparsec
import Cmt.Parser.Config.Format (formatP)
import Cmt.Types.Config

-- predefined
value :: Name -> [Part] -> Parser Config
value name parts = lexeme (char '"' *> takeTill (== '"') <* char '"') >>= getConfig
  where
    getConfig :: Text -> Parser Config
    getConfig template =
        case parseOnly (formatP $ partName <$> parts) template of
            Left _    -> fail $ "Invalid predefined template: " ++ show name
            Right fmt -> pure $ Config (filterParts fmt parts) fmt
    filterParts :: [FormatPart] -> [Part] -> [Part]
    filterParts fps = filter ((`elem` catMaybes (formatName <$> fps)) . partName)

partP :: [Part] -> Parser PreDefinedPart
partP ps =
    stripComments $ do
        name <- pack <$> many' letter <* lexeme (char '=')
        conf <- stripComments (value name ps)
        pure (name, conf)

predefinedPartsP :: [Part] -> Parser PreDefinedParts
predefinedPartsP ps =
    mapFromList <$>
    option [] (stripComments (char '{') *> many' (partP ps) <* stripComments (char '}'))
