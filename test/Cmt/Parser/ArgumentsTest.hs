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
              "parse error"
              (assertEqual "Gives an error" (Error "Could not parse arguments") (parse "-q"))
        ]
