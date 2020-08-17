{-# LANGUAGE NoImplicitPrelude #-}

module Cmt.Types.Config where

import ClassyPrelude   (Eq, Maybe (..), Show, Text)
import Data.Map.Strict (Map)

data Branches
    = Any
    | Deny [Text]
    | Allow [Text]
    deriving (Show, Eq)

type Output = (Name, Text)

type Outputs = Map Name Text

type PreDefinedPart = (Text, Config)

type PreDefinedParts = Map Text Config

type Name = Text

data FormatPart
    = Named Name
    | Literal Text
    deriving (Show, Eq)

data PartType
    = Options [Text]
    | Line
    | Lines
    | Changed
    deriving (Show, Eq)

data Part =
    Part Name
         PartType
    deriving (Show, Eq)

data Config =
    Config Branches
           [Part]
           [FormatPart]
    deriving (Show, Eq)

partName :: Part -> Name
partName (Part name _) = name

formatName :: FormatPart -> Maybe Name
formatName (Named name) = Just name
formatName _            = Nothing
