{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmt.BranchTest where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit

import Cmt.Branch       (check)
import Cmt.Types.Config (BranchName (..), Branches (..))

-- import Test.Tasty.HUnit
test_branch :: TestTree
test_branch =
    testGroup
        "Cmt.Branch"
        [ testGroup
              "Any"
              [ testCase "master" (assertEqual "Allows branch" True (check "master" Any))
              , testCase "develop" (assertEqual "Allows branch" True (check "develop" Any))
              , testCase
                    "feature/test"
                    (assertEqual "Allows branch" True (check "feature/test" Any))
              ]
        , testGroup
              "Allow"
              [ testCase
                    "master"
                    (assertEqual "Allows branch" True (check "master" (Allow [Full "master"])))
              , testCase
                    "develop"
                    (assertEqual "Disallows branch" False (check "develop" (Allow [Full "master"])))
              , testCase
                    "feature/test"
                    (assertEqual
                         "Allows branch"
                         True
                         (check "feature/test" (Allow [Prefix "feature/"])))
              , testCase
                    "multiple: feature/test"
                    (assertEqual
                         "Allows branch"
                         True
                         (check "feature/test" (Allow [Full "master", Prefix "feature/"])))
              , testCase
                    "multiple: feature/test"
                    (assertEqual
                         "Disallows branch"
                         False
                         (check "feature/test" (Allow [Full "master", Prefix "release/"])))
              ]
        , testGroup
              "Deny"
              [ testCase
                    "master"
                    (assertEqual "Disallows branch" False (check "master" (Deny [Full "master"])))
              , testCase
                    "develop"
                    (assertEqual "Allows branch" True (check "develop" (Deny [Full "master"])))
              , testCase
                    "feature/test"
                    (assertEqual
                         "Disallows branch"
                         False
                         (check "feature/test" (Deny [Prefix "feature/"])))
              , testCase
                    "multiple: feature/test"
                    (assertEqual
                         "Disallows branch"
                         False
                         (check "feature/test" (Deny [Full "master", Prefix "feature/"])))
              ]
        ]
