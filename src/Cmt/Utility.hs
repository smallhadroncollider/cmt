{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Cmt.Utility where

import ClassyPrelude

(<?>) :: Maybe a -> Maybe a -> Maybe a
(<?>) Nothing b = b
(<?>) a _       = a
