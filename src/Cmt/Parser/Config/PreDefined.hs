{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}

module Cmt.Parser.Config.PreDefined
    ( predefinedPartsP
    ) where

import ClassyPrelude

import Control.Monad.Fail   (fail)
import Data.Attoparsec.Text

import Cmt.Parser.Attoparsec
import Cmt.Parser.Config.Format (formatP)
import Cmt.Types.Config

-- predefined
value :: Name -> Branches -> [Part] -> Parser Config
value name branches parts = lexeme (char '"' *> takeTill (== '"') <* char '"') >>= getConfig
  where
    getConfig :: Text -> Parser Config
    getConfig template =
        case parseOnly (formatP $ partName <$> parts) template of
            Left _    -> fail $ "Invalid predefined template: " ++ show name
            Right fmt -> pure $ Config branches (filterParts fmt parts) fmt
    filterParts :: [FormatPart] -> [Part] -> [Part]
    filterParts fps = filter ((`elem` catMaybes (formatName <$> fps)) . partName)

partP :: Branches -> [Part] -> Parser PreDefinedPart
partP branches ps =
    stripComments $ do
        name <- pack <$> many' letter <* lexeme (char '=')
        conf <- stripComments (value name branches ps)
        pure (name, conf)

predefinedPartsP :: Branches -> [Part] -> Parser PreDefinedParts
predefinedPartsP branches ps =
    mapFromList <$>
    option [] (stripComments (char '{') *> many' (partP branches ps) <* stripComments (char '}'))
