{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Cmt.Parser.ArgumentsTest where

-- import ClassyPrelude
import Test.Tasty
import Test.Tasty.HUnit

import Cmt                  (Next (..))
import Cmt.Parser.Arguments (parse)

test_config :: TestTree
test_config =
    testGroup
        "Cmt.Parser.Arguments"
        [ testCase "help" (assertEqual "Gives back Help" Help (parse "-h"))
        , testCase "version" (assertEqual "Gives back Version" Version (parse "-v"))
        , testCase
              "config location"
              (assertEqual "Gives back ConfigLocation" ConfigLocation (parse "-c"))
        , testCase "previous" (assertEqual "Gives back Previous" Previous (parse "--prev"))
        , testCase
              "predefined message"
              (assertEqual "Gives back PreDefined and name" (PreDefined "test" []) (parse "-p test"))
        , testCase
              "predefined message plus message"
              (assertEqual "Gives back PreDefined, name and message" (PreDefined "test" [("*","a message")]) (parse "-p test a message"))
        , testCase
              "continue"
              (assertEqual "Gives back empty Continue" (Continue []) (parse ""))
        , testCase
              "continue"
              (assertEqual "Gives back Continue with message" (Continue [("*", "a message")]) (parse "a message"))
        ]
