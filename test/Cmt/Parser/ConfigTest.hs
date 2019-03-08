{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Cmt.Parser.ConfigTest where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit

import Data.FileEmbed (embedFile)

import Cmt.Parser.Config (config, predefined)
import Cmt.Types.Config

basic :: Text
basic = decodeUtf8 $(embedFile "test/data/.cmt")

basicConfig :: Config
basicConfig = Config [Part "Week" Line] [Named "Week", Literal ": ", Named "*", Literal "\n"]

angular :: Text
angular = decodeUtf8 $(embedFile "test/data/.cmt-angular")

comments :: Text
comments = decodeUtf8 $(embedFile "test/data/.cmt-comments")

angularConfig :: Config
angularConfig =
    Config
        [ Part "Type" (Options ["feat", "fix", "docs", "style", "refactor", "test", "chore"])
        , Part "Scope" Changed
        , Part "Short Message" Line
        , Part "Body" Lines
        ]
        [ Named "Type"
        , Literal " ("
        , Named "Scope"
        , Literal "): "
        , Named "Short Message"
        , Literal "\n\n"
        , Named "Body"
        , Literal "\n"
        ]

pre :: Text
pre = decodeUtf8 $(embedFile "test/data/.cmt-predefined")

-- import Test.Tasty.HUnit
test_config :: TestTree
test_config =
    testGroup
        "Cmt.Parser.Config"
        [ testGroup
              "config"
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
        , testGroup
              "predefined"
              [ testCase
                    "angular"
                    (assertEqual "Gives back correct format" (Right []) (predefined comments))
              , testCase
                    "predefined"
                    (assertEqual
                         "Gives back correct format"
                         (Right
                              [ ("vb", "chore (package.yaml): version bump")
                              , ("readme", "docs (README.md): updated readme")
                              ])
                         (predefined pre))
              ]
        ]
