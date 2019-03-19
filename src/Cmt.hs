{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmt
    ( go
    ) where

import ClassyPrelude

import Data.Text        (stripEnd)
import System.Directory (removeFile)
import System.Exit      (exitFailure, exitSuccess)

import Cmt.IO.Config     (load, readCfg, checkFormat)
import Cmt.IO.Git        (commit)
import Cmt.IO.Input      (loop)
import Cmt.Output.Format (format)
import Cmt.Parser.Config (predefined)
import Cmt.Types.Config  (Config, Output)

data Next
    = Previous
    | PreDefined Text [Output]
    | Continue [Output]

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

predef :: Text -> [Output] -> IO ()
predef name output = do
    cfg <- load
    case predefined =<< cfg of
        Left msg -> failure msg
        Right pre ->
            case find ((==) name . fst) pre of
                Nothing       -> failure "No matching predefined message"
                Just (_, cfg) -> display $ checkFormat output cfg

parseArgs :: [Text] -> Next
parseArgs ["--prev"]        = Previous
parseArgs ["-p", name]      = PreDefined name []
parseArgs ["-p", name, msg] = PreDefined name [("*", msg)]
parseArgs ("-p":name:parts) = PreDefined name [("*", unwords parts)]
parseArgs []                = Continue []
parseArgs [msg]             = Continue [("*", msg)]
parseArgs parts             = Continue [("*", unwords parts)]

go :: IO ()
go = do
    next <- parseArgs <$> getArgs
    case next of
        Continue output -> readCfg output >>= display
        Previous        -> previous
        PreDefined name output -> predef name output
