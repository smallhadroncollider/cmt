{-# LANGUAGE NoImplicitPrelude #-}

module Cmt.Parser.Attoparsec where

import ClassyPrelude

import Data.Attoparsec.Text (Parser, char, option, skipSpace)

lexeme :: Parser a -> Parser a
lexeme p = skipSpace *> p <* skipSpace

tchar :: Char -> Parser Text
tchar ch = singleton <$> char ch

chopt :: Char -> Parser Text
chopt ch = option "" (tchar ch)
