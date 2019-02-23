{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmt.Parser.Config
    ( config
    ) where

import ClassyPrelude

import Data.Attoparsec.Text

import Cmt.Parser.Attoparsec (chopt, lexeme, tnotChar)
import Cmt.Types.Config

word :: Parser Text
word = pack <$> many1 (letter <|> space)

valid :: [Name] -> Parser Text
valid names = choice $ "*" : (string <$> names)

formatNamedP :: [Name] -> Parser FormatPart
formatNamedP names = Named <$> (string "${" *> valid names <* char '}')

formatLiteralP :: Parser FormatPart
formatLiteralP = Literal <$> (singleton <$> anyChar)

formatP :: [Name] -> Parser Format
formatP names = lexeme $ many1 (formatNamedP names <|> formatLiteralP)

-- part types
lineP :: Parser PartType
lineP = char '@' $> Line

linesP :: Parser PartType
linesP = string "!@" $> Lines

listItemP :: Parser Text
listItemP = lexeme $ char '"' *> tnotChar '"' <* char '"' <* chopt ','

listP :: Parser PartType
listP = Options <$> (char '[' *> many' listItemP <* char ']')

-- name
nameP :: Parser Text
nameP = char '"' *> word <* char '"' <* lexeme (char '=')

-- part
partP :: Parser Part
partP = lexeme $ Part <$> nameP <*> (listP <|> lineP <|> linesP)

partsP :: Parser [Part]
partsP = lexeme $ char '{' *> many' partP <* char '}'

configP :: Parser Config
configP = do
    parts <- partsP
    format <- formatP $ partName <$> parts
    _ <- endOfInput
    pure $ Config parts format

-- run parser
config :: Text -> Either Text Config
config cfg =
    case parseOnly configP cfg of
        Right c -> Right c
        Left _ ->
            Left "Could not parse config. Check that your format doesn't contain any invalid parts."
