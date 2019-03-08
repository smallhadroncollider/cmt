{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmt.Parser.Config.Parts
    ( partsP
    ) where

import ClassyPrelude

import Data.Attoparsec.Text

import Cmt.Parser.Attoparsec
import Cmt.Types.Config

-- input parts
changedP :: Parser PartType
changedP = char '%' $> Changed

lineP :: Parser PartType
lineP = char '@' $> Line

linesP :: Parser PartType
linesP = string "!@" $> Lines

listItemP :: Parser Text
listItemP = stripComments $ char '"' *> tnotChar '"' <* char '"' <* chopt ','

listP :: Parser PartType
listP = Options <$> (char '[' *> many' listItemP <* char ']')

-- name
nameP :: Parser Text
nameP = char '"' *> word <* char '"' <* lexeme (char '=')

-- part
partP :: Parser Part
partP = stripComments $ Part <$> nameP <*> (listP <|> lineP <|> linesP <|> changedP)

partsP :: Parser [Part]
partsP = stripComments $ stripComments (char '{') *> many' partP <* stripComments (char '}')
