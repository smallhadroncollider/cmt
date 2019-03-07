{-# LANGUAGE NoImplicitPrelude #-}

module Cmt.Parser.Options
    ( parse
    ) where

import ClassyPrelude

import Data.Attoparsec.Text (Parser, char, decimal, many', many1, parseOnly)

import Cmt.Parser.Attoparsec (lexeme)

commaP :: Parser ()
commaP = void $ many' (lexeme $ char ',')

optionsP :: Parser [Int]
optionsP = lexeme . many1 $ commaP *> decimal <* commaP

-- run parser
parse :: Text -> Maybe [Int]
parse options =
    case parseOnly optionsP options of
        Right c -> Just c
        Left _  -> Nothing
