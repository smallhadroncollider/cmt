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
import System.Directory (doesFileExist, removeFile)
import System.Exit      (ExitCode (..), exitFailure, exitSuccess)

import Cmt.IO.CLI           (blank, errorMessage, header, mehssage, message)
import Cmt.IO.Config        (checkFormat, findFile, load, readCfg)
import Cmt.IO.Git           (commit)
import Cmt.IO.Input         (loop)
import Cmt.Output.Format    (format)
import Cmt.Parser.Arguments (parse)
import Cmt.Parser.Config    (predefined)
import Cmt.Types.App        (App, settingsDryRun)
import Cmt.Types.Config     (Config, Outputs)
import Cmt.Types.Next       (Next (..))

helpText :: Text
helpText = decodeUtf8 $(embedFile "templates/usage.txt")

backup :: FilePath
backup = ".cmt.bkp"

failure :: Text -> App
failure msg = errorMessage msg >> lift exitFailure

dryRun :: Text -> App
dryRun txt = do
    header "Result"
    blank
    message txt
    blank
    lift $ writeFile backup (encodeUtf8 txt)
    mehssage "run: cmt --prev to commit"
    lift $ exitSuccess

commitRun :: Text -> App
commitRun txt = do
    commited <- lift (commit $ stripEnd txt)
    case commited of
        ExitSuccess -> lift exitSuccess
        ExitFailure _ -> do
            writeFile backup (encodeUtf8 txt)
            lift exitFailure

send :: Text -> App
send txt = do
    dry <- asks settingsDryRun
    bool commitRun dryRun dry txt

display :: Either Text (Config, Outputs) -> App
display (Left err) = putStrLn err
display (Right (cfg, output)) = do
    parts <- lift $ loop cfg
    send $ format cfg (output ++ parts)

previous :: App
previous = do
    exists <- lift $ doesFileExist backup
    if exists
        then (do txt <- decodeUtf8 <$> readFile backup
                 lift $ removeFile backup
                 send txt)
        else (failure "No previous commit attempts")

predef :: Text -> Outputs -> App
predef name output = do
    cfg <- lift load
    case predefined =<< cfg of
        Left msg -> failure msg
        Right pre ->
            case lookup name pre of
                Nothing -> failure "No matching predefined message"
                Just cf -> display $ checkFormat output cf

configLocation :: App
configLocation = do
    file <- lift findFile
    case file of
        Just path -> putStrLn (pack path)
        Nothing   -> failure ".cmt file not found"

next :: Next -> App
next (Continue output)        = lift (readCfg output) >>= display
next Previous                 = previous
next (PreDefined name output) = predef name output
next Version                  = putStrLn "0.7.0"
next ConfigLocation           = configLocation
next Help                     = putStrLn helpText

go :: IO ()
go = do
    ready <- parse . unwords <$> getArgs
    case ready of
        Right (settings, nxt) -> runReaderT (next nxt) settings
        Left err              -> putStrLn err >> exitFailure
