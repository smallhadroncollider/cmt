{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmt.Branch
    ( check
    ) where

import ClassyPrelude

import Cmt.Types.Config (BranchName (..), Branches (..))

matches :: Text -> BranchName -> Bool
matches actual (Full name)   = name == actual
matches actual (Prefix name) = name `isPrefixOf` actual

check :: Text -> Branches -> Bool
check _ Any                   = True
check actual (Allow branches) = any (matches actual) branches
check actual (Deny branches)  = not $ any (matches actual) branches
