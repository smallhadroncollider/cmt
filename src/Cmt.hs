{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Cmt
    ( go
    ) where

import ClassyPrelude

import Data.FileEmbed   (embedFile)
import Data.Text        (stripEnd)
import System.Directory (removeFile)
import System.Exit      (ExitCode (..), exitFailure, exitSuccess)

import Cmt.IO.Config     (checkFormat, findFile, load, readCfg)
import Cmt.IO.Git        (commit)
import Cmt.IO.Input      (loop)
import Cmt.Output.Format (format)
import Cmt.Parser.Config (predefined)
import Cmt.Types.Config  (Config, Outputs)

data Next
    = Previous
    | PreDefined Text
                 Outputs
    | Continue Outputs
    | Version
    | ConfigLocation
    | Help

helpText :: Text
helpText = decodeUtf8 $(embedFile "templates/usage.txt")

backup :: FilePath
backup = ".cmt.bkp"

failure :: Text -> IO ()
failure msg = putStrLn msg >> exitFailure

send :: Text -> IO ()
send txt = do
    commited <- commit $ stripEnd txt
    case commited of
        ExitSuccess -> exitSuccess
        ExitFailure _ -> do
            writeFile backup (encodeUtf8 txt)
            exitFailure

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

predef :: Text -> Outputs -> IO ()
predef name output = do
    cfg <- load
    case predefined =<< cfg of
        Left msg -> failure msg
        Right pre ->
            case lookup name pre of
                Nothing -> failure "No matching predefined message"
                Just cf -> display $ checkFormat output cf

configLocation :: IO ()
configLocation = do
    file <- findFile
    case file of
        Just path -> putStrLn (pack path)
        Nothing   -> putStrLn ".cmt file not found"

parseArgs :: [Text] -> Next
parseArgs ["-h"]            = Help
parseArgs ["-v"]            = Version
parseArgs ["-c"]            = ConfigLocation
parseArgs ["--prev"]        = Previous
parseArgs ["-p", name]      = PreDefined name []
parseArgs ["-p", name, msg] = PreDefined name [("*", msg)]
parseArgs ("-p":name:parts) = PreDefined name [("*", unwords parts)]
parseArgs []                = Continue []
parseArgs [msg]             = Continue [("*", msg)]
parseArgs parts             = Continue [("*", unwords parts)]

next :: Next -> IO ()
next (Continue output)        = readCfg output >>= display
next Previous                 = previous
next (PreDefined name output) = predef name output
next Version                  = putStrLn "0.6.0"
next ConfigLocation           = configLocation
next Help                     = putStrLn helpText

go :: IO ()
go = next =<< parseArgs <$> getArgs
