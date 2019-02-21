{-# LANGUAGE NoImplicitPrelude #-}

module Cmt.Types.Config where

import ClassyPrelude (Maybe (..), Show, Text)

type Name = Text

data FormatPart
    = Named Name
    | Literal Text
    deriving (Show)

type Format = [FormatPart]

data PartType
    = Options [Text]
    | Line
    | Lines
    deriving (Show)

data Part =
    Part Name
         PartType
    deriving (Show)

data Config =
    Config [Part]
           Format
    deriving (Show)

partName :: Part -> Name
partName (Part name _) = name

formatName :: FormatPart -> Maybe Name
formatName (Named name) = Just name
formatName _            = Nothing
