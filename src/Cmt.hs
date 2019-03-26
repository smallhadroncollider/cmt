{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Cmt
    ( go
    ) where

import ClassyPrelude

import Data.Text        (stripEnd)
import System.Directory (removeFile)
import System.Exit      (exitFailure, exitSuccess)

import Cmt.IO.Config     (load, readCfg)
import Cmt.IO.Git        (commit)
import Cmt.IO.Input      (loop)
import Cmt.Output.Format (format)
import Cmt.Parser.Config (predefined)
import Cmt.Types.Config  (Config, Outputs)

data Next
    = Previous
    | PreDefined Text
    | Continue Outputs

backup :: FilePath
backup = ".cmt.bkp"

failure :: Text -> IO ()
failure msg = putStrLn msg >> exitFailure

send :: Text -> IO ()
send txt = do
    commited <- commit $ stripEnd txt
    case commited of
        Right msg -> putStrLn msg >> exitSuccess
        Left msg -> do
            writeFile backup (encodeUtf8 txt)
            failure msg

display :: Either Text (Config, Outputs) -> IO ()
display (Left err) = putStrLn err
display (Right (cfg, output)) = do
    parts <- loop cfg
    send $ format cfg (output ++ parts)

previous :: IO ()
previous = do
    txt <- decodeUtf8 <$> readFile backup
    removeFile backup
    send txt

predef :: Text -> IO ()
predef name = do
    cfg <- load
    case predefined =<< cfg of
        Left msg -> failure msg
        Right pre ->
            case lookup name pre of
                Nothing  -> failure "No matching predefined message"
                Just txt -> send txt

parseArgs :: [Text] -> Next
parseArgs ["--prev"]   = Previous
parseArgs ["-p", name] = PreDefined name
parseArgs []           = Continue []
parseArgs [msg]        = Continue [("*", msg)]
parseArgs parts        = Continue [("*", unwords parts)]

go :: IO ()
go = do
    next <- parseArgs <$> getArgs
    case next of
        Continue output -> readCfg output >>= display
        Previous        -> previous
        PreDefined name -> predef name
