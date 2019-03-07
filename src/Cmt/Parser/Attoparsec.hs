{-# LANGUAGE NoImplicitPrelude #-}

module Cmt.Parser.Attoparsec
    ( lexeme
    ) where

import ClassyPrelude

import Data.Attoparsec.Text

lexeme :: Parser a -> Parser a
lexeme p = skipSpace *> p <* skipSpace
