{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmt.IO.Git
    ( commit
    ) where

import ClassyPrelude

import System.Process (readCreateProcessWithExitCode, shell)

commit :: Text -> IO Text
commit message = do
    let msg = "git commit -m '" <> unpack message <> "'"
    (_, out, _) <- readCreateProcessWithExitCode (shell msg) ""
    pure $ pack out
