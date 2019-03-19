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

preWithVars :: Text
preWithVars = decodeUtf8 $(embedFile "test/data/.cmt-predefined-with-vars")

preConfig :: [PreDefinedPart]
preConfig =
    [ ("vb", Config parts [ Literal "chore (package.yaml): version bump"])
    , ("readme", Config parts [ Literal "docs (README.md): updated readme"])
    ]
  where
    parts =
      [ Part "Type" (Options ["feat", "fix", "docs", "style", "refactor", "test", "chore"])
      , Part "Scope" Changed
      , Part "Short Message" Line
      , Part "Body" Lines
      ]

preWithVarsConfig :: [PreDefinedPart]
preWithVarsConfig =
    [ ("vb", Config parts [ Literal "chore (", Named "Scope", Literal "): version bump"])
    , ("readme", Config parts [ Literal "docs (README.md): ", Named "Short Message"])
    ]
  where
    parts =
      [ Part "Type" (Options ["feat", "fix", "docs", "style", "refactor", "test", "chore"])
      , Part "Scope" Changed
      , Part "Short Message" Line
      , Part "Body" Lines
      ]

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
                        (Right preConfig)
                        (predefined pre))
             , testCase
                    "predefined with variables"
                    (assertEqual
                         "Gives back correct format"
                         (Right preWithVarsConfig)
                         (predefined preWithVars))
              ]
        ]
