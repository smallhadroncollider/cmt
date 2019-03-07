{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmt.IO.Git
    ( commit
    , changed
    ) where

import ClassyPrelude

import System.Process (readCreateProcessWithExitCode, shell)

commit :: Text -> IO Text
commit message = do
    let msg = "git commit -m '" <> unpack message <> "'"
    (_, out, err) <- readCreateProcessWithExitCode (shell msg) ""
    pure $ unlines (pack <$> filter (not . null) [out, err])

changed :: IO [Text]
changed = do
    let msg = "git diff --name-only --cached"
    (_, out, _) <- readCreateProcessWithExitCode (shell msg) ""
    pure . lines $ pack out
