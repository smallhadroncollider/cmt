{-# LANGUAGE OverloadedLists #-}

module Cmt.Parser.Arguments
    ( parse
    ) where

import ClassyPrelude

import Data.Attoparsec.Text hiding (parse)

import Cmt.Parser.Attoparsec (ifP, lexeme, wordP)
import Cmt.Types.App         (Settings (Settings))
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

settingsP :: Parser Next -> Parser (Settings, Next)
settingsP p = do
    dry <- ifP (string "--dry-run" *> skipSpace)
    colour <- not <$> ifP (string "--no-color" *> skipSpace)
    next <- p
    let settings = Settings dry colour
    pure (settings, next)

argumentsP :: Parser (Settings, Next)
argumentsP =
    lexeme $
    settingsP (helpP <|> versionP <|> configLocationP <|> previousP <|> preDefinedP <|> continueP)

-- run parser
parse :: Text -> Either Text (Settings, Next)
parse arguments =
    case parseOnly argumentsP arguments of
        Right c -> Right c
        Left _  -> Left "Could not parse arguments"
