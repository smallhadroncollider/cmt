{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmt.Parser.Config
    ( config
    ) where

import ClassyPrelude

import Data.Attoparsec.Text

import Cmt.Types.Config

-- useful bits
lexeme :: Parser a -> Parser a
lexeme p = skipSpace *> p <* skipSpace

tchar :: Char -> Parser Text
tchar ch = singleton <$> char ch

chopt :: Char -> Parser Text
chopt ch = option "" (tchar ch)

tnotChar :: Char -> Parser Text
tnotChar c = pack <$> many1 (notChar c)

commentP :: Parser ()
commentP = lexeme $ many' (char '#' *> many' (notChar '\n') *> char '\n') $> ()

stripComments :: Parser a -> Parser a
stripComments p = lexeme $ commentP *> p <* commentP

word :: Parser Text
word = pack <$> many1 (letter <|> space)

valid :: [Name] -> Parser Text
valid names = choice $ "*" : (string <$> names)

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

-- input parts
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
partP = stripComments $ Part <$> nameP <*> (listP <|> lineP <|> linesP)

partsP :: Parser [Part]
partsP = stripComments $ stripComments (char '{') *> many' partP <* stripComments (char '}')

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
