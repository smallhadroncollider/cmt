{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmt.Parser.Config.PreDefined
    ( predefinedPartsP
    ) where

import ClassyPrelude

import Data.Attoparsec.Text

import Cmt.Parser.Attoparsec
import Cmt.Parser.Config.Format (formatP)
import Cmt.Types.Config

-- predefined
value :: [Part] -> Parser Config
value parts = do
  template <- lexeme $ char '"' *> takeTill (== '"') <* char '"'
  Config parts <$> getFormat template
  where
    getFormat :: Text -> Parser [FormatPart]
    getFormat template = case parseOnly (formatP $ partName <$> parts) template of
      Left _ -> fail "invalid predefined template"
      Right format -> pure format

partP :: [Part] -> Parser PreDefinedPart
partP ps =
  stripComments $ (,)
    <$> (pack <$> many' letter <* lexeme (char '='))
    <*> stripComments (value ps)

predefinedPartsP :: [Part] -> Parser [PreDefinedPart]
predefinedPartsP ps =
  option [] $
    stripComments (char '{') *> many' (partP ps) <* stripComments (char '}')

