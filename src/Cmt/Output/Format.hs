{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}

module Cmt.Output.Format
    ( format
    ) where

import ClassyPrelude

import Cmt.Types.Config

tidy :: Outputs -> FormatPart -> Maybe Text
tidy _ (Literal c)      = pure c
tidy parts (Named name) = lookup name parts

format :: Config -> Outputs -> Text
format (Config _ fmt) parts = concat . catMaybes $ tidy parts <$> fmt
