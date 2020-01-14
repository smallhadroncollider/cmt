{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}

module Cmt.Parser.Arguments
    ( parse
    ) where

import ClassyPrelude

import Data.Attoparsec.Text hiding (parse)

import Cmt.Parser.Attoparsec (ifP, lexeme, wordP)
import Cmt.Types.Config      (Outputs)
import Cmt.Types.Next        (Next (..))

outputsP :: Parser Outputs
outputsP =
    lexeme $ do
        message <- takeText
        pure [("*", message)]

emptyOutputsP :: Parser Outputs
emptyOutputsP = endOfInput $> []

continueP :: Parser Next
continueP = Continue <$> (emptyOutputsP <|> outputsP)

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

dryRunP :: Parser Next -> Parser Next
dryRunP p = do
    dry <- ifP (string "--dry-run" *> skipSpace)
    next <- p
    pure $ bool next (DryRun next) dry

argumentsP :: Parser Next
argumentsP =
    lexeme
        (helpP <|> versionP <|> configLocationP <|>
         dryRunP (previousP <|> preDefinedP <|> continueP))

-- run parser
parse :: Text -> Next
parse arguments =
    case parseOnly argumentsP arguments of
        Right c -> c
        Left _  -> Error "Could not parse arguments"
