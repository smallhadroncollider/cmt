{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmt.Output.Format
    ( format
    ) where

import ClassyPrelude

import Cmt.Types.Config

tidy :: [(Name, Text)] -> FormatPart -> Text
tidy _ (Literal c) = c
tidy parts (Named name) =
    case find ((== name) . fst) parts of
        Just (_, t) -> t
        Nothing     -> ""

format :: Config -> [(Name, Text)] -> Text
format (Config _ fmt) parts = concat $ tidy parts <$> fmt
