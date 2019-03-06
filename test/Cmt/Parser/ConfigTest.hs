{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Cmt.Parser.ConfigTest where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit

import Data.FileEmbed (embedFile)

import Cmt.Parser.Config (config)
import Cmt.Types.Config

basic :: Text
basic = decodeUtf8 $(embedFile "test/data/.cmt")

basicConfig :: Config
basicConfig =
    Config [Part "Week" Line] [Named "Week", Literal ":", Literal " ", Named "*", Literal "\n"]

angular :: Text
angular = decodeUtf8 $(embedFile "test/data/.cmt-angular")

comments :: Text
comments = decodeUtf8 $(embedFile "test/data/.cmt-comments")

angularConfig :: Config
angularConfig =
    Config
        [ Part "Type" (Options ["feat", "fix", "docs", "style", "refactor", "test", "chore"])
        , Part "Scope" Line
        , Part "Short Message" Line
        , Part "Body" Lines
        ]
        [ Named "Type"
        , Literal " "
        , Literal "("
        , Named "Scope"
        , Literal ")"
        , Literal ":"
        , Literal " "
        , Named "Short Message"
        , Literal "\n"
        , Literal "\n"
        , Named "Body"
        , Literal "\n"
        ]

-- import Test.Tasty.HUnit
test_config :: TestTree
test_config =
    testGroup
        "Cmt.Parser.Config"
        [ testCase
              "basic"
              (assertEqual "Gives back correct format" (Right basicConfig) (config basic))
        , testCase
              "angular"
              (assertEqual "Gives back correct format" (Right angularConfig) (config angular))
        , testCase
              "comments"
              (assertEqual "Gives back correct format" (Right angularConfig) (config comments))
        ]
