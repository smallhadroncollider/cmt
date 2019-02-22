{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmt.Parser.Attoparsec where

import ClassyPrelude

import Data.Attoparsec.Text

lexeme :: Parser a -> Parser a
lexeme p = skipSpace *> p <* skipSpace

tchar :: Char -> Parser Text
tchar ch = singleton <$> char ch

chopt :: Char -> Parser Text
chopt ch = option "" (tchar ch)

tnotChar :: Char -> Parser Text
tnotChar c = pack <$> many1 (notChar c)
