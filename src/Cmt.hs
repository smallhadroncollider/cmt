{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmt
    ( go
    ) where

import ClassyPrelude

import Cmt.IO.Config     (load)
import Cmt.IO.Git        (commit)
import Cmt.IO.Input      (loop)
import Cmt.Output.Format (format)
import Cmt.Types.Config  (Config, Output)

display :: Either Text (Config, [Output]) -> IO ()
display (Left err) = putStrLn err
display (Right (cfg, output)) = do
    parts <- loop cfg
    let txt = format cfg (output ++ parts)
    commit txt >>= putStrLn

go :: IO ()
go = load >>= display
