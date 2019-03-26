{-# LANGUAGE NoImplicitPrelude #-}

module Cmt.Types.Config where

import ClassyPrelude (Eq, Maybe (..), Show, Text)

type Output = (Name, Text)

type PreDefinedPart = (Text, Config)

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
    Config [Part]
           [FormatPart]
    deriving (Show, Eq)

partName :: Part -> Name
partName (Part name _) = name

formatName :: FormatPart -> Maybe Name
formatName (Named name) = Just name
formatName _            = Nothing
