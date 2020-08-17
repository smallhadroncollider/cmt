{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmt.Parser.Config.Branches
    ( branchesP
    ) where

import ClassyPrelude

import Data.Attoparsec.Text

import Cmt.Parser.Attoparsec
import Cmt.Types.Config      (BranchName (..), Branches (..))

charP :: Parser Char
charP = digit <|> letter <|> choice (char <$> "-_/")

nameP :: Parser BranchName
nameP = do
    name <- pack <$> many1 charP
    typ <- bool Full Prefix <$> ifP (void $ char '*')
    pure $ typ name

nameListP :: Parser [BranchName]
nameListP = char '[' *> nameP `sepBy` (char ',') <* char ']'

allowP :: Parser Branches
allowP = Allow <$> nameListP

denyP :: Parser Branches
denyP = Deny <$> (string "!" *> nameListP)

branchesP' :: Parser Branches
branchesP' = denyP <|> allowP

-- input parts
branchesP :: Parser Branches
branchesP = stripComments $ option Any branchesP'
