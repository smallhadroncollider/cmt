{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmt.Parser.Config.PreDefined
    ( predefinedPartsP
    ) where

import ClassyPrelude

import Data.Attoparsec.Text

import Cmt.Parser.Attoparsec
import Cmt.Types.Config

-- predefined
value :: Parser Text
value = lexeme $ char '"' *> tnotChar '"' <* char '"'

partP :: Parser PreDefinedPart
partP = stripComments $ (,) <$> (pack <$> many' letter <* lexeme (char '=')) <*> value

predefinedPartsP :: Parser [PreDefinedPart]
predefinedPartsP =
    stripComments $ option [] (stripComments (char '{') *> many' partP <* stripComments (char '}'))
