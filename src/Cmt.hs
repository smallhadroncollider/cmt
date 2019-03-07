{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmt
    ( go
    ) where

import ClassyPrelude

import System.Directory (removeFile)

import Cmt.IO.Config     (load)
import Cmt.IO.Git        (commit)
import Cmt.IO.Input      (loop)
import Cmt.Output.Format (format)
import Cmt.Types.Config  (Config, Output)

data Next
    = Previous
    | Continue [Output]

backup :: FilePath
backup = ".cmt.bkp"

send :: Text -> IO ()
send txt = do
    commited <- commit txt
    case commited of
        Right msg -> putStrLn msg
        Left msg -> do
            writeFile backup (encodeUtf8 txt)
            putStrLn msg

display :: Either Text (Config, [Output]) -> IO ()
display (Left err) = putStrLn err
display (Right (cfg, output)) = do
    parts <- loop cfg
    send $ format cfg (output ++ parts)

previous :: IO ()
previous = do
    txt <- decodeUtf8 <$> readFile backup
    removeFile backup
    send txt

parseArgs :: [Text] -> Next
parseArgs ["--prev"] = Previous
parseArgs []         = Continue []
parseArgs [msg]      = Continue [("*", msg)]
parseArgs parts      = Continue [("*", unwords parts)]

go :: IO ()
go = do
    next <- parseArgs <$> getArgs
    case next of
        Continue output -> load output >>= display
        Previous        -> previous
