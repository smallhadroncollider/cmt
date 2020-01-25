{-# LANGUAGE NoImplicitPrelude #-}

module Cmt.Parser.Arguments
    ( parse
    ) where

import ClassyPrelude

import Data.Attoparsec.Text hiding (parse)

import Cmt                   (Next (..))
import Cmt.Parser.Attoparsec (lexeme)

configLocationP :: Parser Next
configLocationP = string "-c" $> ConfigLocation

versionP :: Parser Next
versionP = string "-v" $> Version

helpP :: Parser Next
helpP = string "-h" $> Help

argumentsP :: Parser Next
argumentsP = lexeme (helpP <|> versionP <|> configLocationP)

-- run parser
parse :: Text -> Next
parse arguments =
    case parseOnly argumentsP arguments of
        Right c -> c
        Left _  -> Error "Could not parse arguments"
