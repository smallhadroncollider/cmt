{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmt.IO.Git
    ( commit
    , changed
    ) where

import ClassyPrelude

import System.Exit    (ExitCode)
import System.Process (readCreateProcessWithExitCode, shell, spawnCommand, waitForProcess)

-- copied from https://hackage.haskell.org/package/posix-escape
escapeChar :: Char -> String
escapeChar '\0' = ""
escapeChar '\'' = "'\"'\"'"
escapeChar x    = [x]

escape :: String -> String
escape xs = "'" ++ concatMap escapeChar xs ++ "'"

commit :: Text -> IO ExitCode
commit message = do
    let msg = "git commit -m" <> escape (unpack message)
    waitForProcess =<< spawnCommand msg

changed :: IO [Text]
changed = do
    let msg = "git diff --name-only --cached"
    (_, out, _) <- readCreateProcessWithExitCode (shell msg) ""
    pure . lines $ pack out
