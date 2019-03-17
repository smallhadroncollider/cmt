{-# LANGUAGE NoImplicitPrelude #-}

module Cmt.Output.Format
    ( format
    ) where

import ClassyPrelude

import Cmt.Types.Config

tidy :: [Output] -> FormatPart -> Maybe Text
tidy _ (Literal c)      = Just c
tidy parts (Named name) = lookup name parts

format :: Config -> [Output] -> Text
format (Config _ fmt) parts = concat . catMaybes $ tidy parts <$> fmt
