{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmt.IO.Git
    ( commit
    , changed
    ) where

import ClassyPrelude

import System.Exit    (ExitCode (ExitFailure))
import System.Process (readCreateProcessWithExitCode, shell)

-- copied from https://hackage.haskell.org/package/posix-escape
escape :: String -> String
escape xs = "'" ++ concatMap f xs ++ "'"
  where
    f '\0' = ""
    f '\'' = "'\"'\"'"
    f x    = [x]

commit :: Text -> IO (Either Text Text)
commit message = do
    let msg = "git commit -m" <> escape (unpack message)
    (code, out, err) <- readCreateProcessWithExitCode (shell msg) ""
    let output = unlines (pack <$> filter (not . null) [out, err])
    pure $
        case code of
            ExitFailure _ -> Left output
            _             -> Right output

changed :: IO [Text]
changed = do
    let msg = "git diff --name-only --cached"
    (_, out, _) <- readCreateProcessWithExitCode (shell msg) ""
    pure . lines $ pack out
