{-# LANGUAGE OverloadedLists #-}

module Cmt.Parser.ArgumentsTest where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit

import Cmt.Parser.Arguments (parse)
import Cmt.Types.App        (Settings (Settings), defaultSettings)
import Cmt.Types.Next       (Next (..))

test_config :: TestTree
test_config =
    testGroup
        "Cmt.Parser.Arguments"
        [ testGroup
              "single arguments"
              [ testCase
                    "help"
                    (assertEqual "Gives back Help" (Right (defaultSettings, Help)) (parse "-h"))
              , testCase
                    "version"
                    (assertEqual
                         "Gives back Version"
                         (Right (defaultSettings, Version))
                         (parse "-v"))
              , testCase
                    "config location"
                    (assertEqual
                         "Gives back ConfigLocation"
                         (Right (defaultSettings, ConfigLocation))
                         (parse "-c"))
              , testCase
                    "previous"
                    (assertEqual
                         "Gives back Previous"
                         (Right (defaultSettings, Previous))
                         (parse "--prev"))
              ]
        , testGroup
              "PreDefined"
              [ testCase
                    "predefined message"
                    (assertEqual
                         "Gives back PreDefined and name"
                         (Right (defaultSettings, PreDefined "test" []))
                         (parse "-p test"))
              , testCase
                    "predefined message plus message"
                    (assertEqual
                         "Gives back PreDefined, name and message"
                         (Right (defaultSettings, PreDefined "test" [("*", "a message")]))
                         (parse "-p test a message"))
              ]
        , testGroup
              "Continue"
              [ testCase
                    "continue"
                    (assertEqual
                         "Gives back empty Continue"
                         (Right (defaultSettings, Continue []))
                         (parse ""))
              , testCase
                    "continue"
                    (assertEqual
                         "Gives back Continue with message"
                         (Right (defaultSettings, Continue [("*", "a message")]))
                         (parse "a message"))
              ]
        , testGroup
              "Settings"
              [ testCase
                    "previous dryn run"
                    (assertEqual
                         "Gives back Previous"
                         (Right (Settings True True, Previous))
                         (parse "--dry-run --prev"))
              , testCase
                    "predefined message plus message"
                    (assertEqual
                         "Gives back PreDefined, name and message"
                         (Right (Settings True True, PreDefined "test" [("*", "a message")]))
                         (parse "--dry-run -p test a message"))
              , testCase
                    "continue"
                    (assertEqual
                         "Gives back Continue with message"
                         (Right (Settings True True, Continue [("*", "a message")]))
                         (parse "--dry-run a message"))
              ]
        ]
