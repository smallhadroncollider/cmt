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
import Cmt.Types.Config  (Config)

display :: Either Text Config -> IO ()
display (Left err)  = putStrLn err
display (Right cfg) = format cfg <$> loop cfg >>= commit >>= putStrLn

go :: IO ()
go = load >>= display
