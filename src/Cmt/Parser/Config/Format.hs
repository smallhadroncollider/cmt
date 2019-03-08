{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmt.Parser.Config.Format
    ( formatP
    ) where

import ClassyPrelude

import Data.Attoparsec.Text

import Cmt.Parser.Attoparsec
import Cmt.Types.Config

-- format parts
merge :: [FormatPart] -> FormatPart -> [FormatPart]
merge ps (Literal str) = maybe [Literal str] merge' (fromNullable ps)
  where
    merge' ps' =
        case last ps' of
            Literal prev -> init ps' <> [Literal (prev <> str)]
            _            -> ps <> [Literal str]
merge ps p = ps <> [p]

smoosh :: [FormatPart] -> [FormatPart]
smoosh = foldl' merge []

formatNamedP :: [Name] -> Parser FormatPart
formatNamedP names = Named <$> (string "${" *> valid names <* char '}')

formatLiteralP :: Parser FormatPart
formatLiteralP = Literal <$> (singleton <$> anyChar)

formatP :: [Name] -> Parser [FormatPart]
formatP names = smoosh <$> stripComments (many1 (formatNamedP names <|> formatLiteralP))
