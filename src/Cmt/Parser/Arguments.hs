{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}

module Cmt.Parser.Arguments
    ( parse
    ) where

import ClassyPrelude

import Data.Attoparsec.Text hiding (parse)

import Cmt                   (Next (..))
import Cmt.Parser.Attoparsec (lexeme, wordP)
import Cmt.Types.Config (Outputs)

outputsP :: Parser Outputs
outputsP = lexeme $ do
  message <- takeText
  pure $ [("*", message)]

emptyOutputsP :: Parser Outputs
emptyOutputsP = endOfInput $> []

preDefinedP :: Parser Next
preDefinedP = PreDefined <$> (string "-p" *> skipSpace *> wordP) <*> (emptyOutputsP <|> outputsP)

previousP :: Parser Next
previousP = string "--prev" $> Previous

configLocationP :: Parser Next
configLocationP = string "-c" $> ConfigLocation

versionP :: Parser Next
versionP = string "-v" $> Version

helpP :: Parser Next
helpP = string "-h" $> Help

argumentsP :: Parser Next
argumentsP = lexeme (helpP <|> versionP <|> configLocationP <|> previousP <|> preDefinedP)

-- run parser
parse :: Text -> Next
parse arguments =
    case parseOnly argumentsP arguments of
        Right c -> c
        Left _  -> Error "Could not parse arguments"
