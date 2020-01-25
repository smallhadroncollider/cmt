{-# LANGUAGE NoImplicitPrelude #-}

module Cmt.Types.Next
    ( Next(..)
    ) where

import ClassyPrelude

import Cmt.Types.Config (Outputs)

data Next
    = Previous
    | PreDefined Text
                 Outputs
    | Continue Outputs
    | Version
    | ConfigLocation
    | Help
    | Error Text
    | DryRun Next
    deriving (Eq, Show)
