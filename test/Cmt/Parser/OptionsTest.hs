{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmt.Parser.OptionsTest where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit

import Cmt.Parser.Options (parse)

-- import Test.Tasty.HUnit
test_config :: TestTree
test_config =
    testGroup
        "Cmt.Parser.Options"
        [ testCase
              "basic"
              (assertEqual "Gives back correct format" (Just [1, 2, 3]) (parse "1, 2, 3"))
        , testCase "no spaces" (assertEqual "Gives back correct format" (Just [1, 3]) (parse "1,3"))
        , testCase
              "missing number"
              (assertEqual "Gives back correct format" (Just [12, 3]) (parse "12, , 3"))
        , testCase
              "starting comma"
              (assertEqual "Gives back correct format" (Just [12, 3]) (parse ",12, 3"))
        , testCase
              "mess"
              (assertEqual
                   "Gives back correct format"
                   (Just [12, 4, 3])
                   (parse ",,  , 12,,4   ,  3,   , "))
        ]
