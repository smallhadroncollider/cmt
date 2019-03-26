{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}

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

predefinedPartsP :: Parser PreDefinedParts
predefinedPartsP =
    stripComments $
    mapFromList <$> option [] (stripComments (char '{') *> many' partP <* stripComments (char '}'))
