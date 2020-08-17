{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmt.IO.CLI
    ( blank
    , message
    , mehssage
    , header
    , errorMessage
    ) where

import ClassyPrelude

import Data.Text.IO        (hPutStrLn)
import System.Console.ANSI (Color (Blue, Magenta, Red, Yellow), ColorIntensity (Dull),
                            ConsoleLayer (Foreground), SGR (Reset, SetColor), hSetSGR)

import Cmt.Types.App (App, settingsColourize)

setSGR :: Handle -> [SGR] -> App ()
setSGR hndl settings = do
    colourize <- asks settingsColourize
    when colourize $ lift (hSetSGR hndl settings)

blank :: App ()
blank = putStrLn ""

message :: Text -> App ()
message msg = do
    setSGR stdout [SetColor Foreground Dull Blue]
    putStrLn msg
    setSGR stdout [Reset]

mehssage :: Text -> App ()
mehssage msg = do
    setSGR stdout [SetColor Foreground Dull Yellow]
    putStrLn msg
    setSGR stdout [Reset]

header :: Text -> App ()
header msg = do
    setSGR stdout [SetColor Foreground Dull Magenta]
    putStrLn $ "*** " ++ msg ++ " ***"
    setSGR stdout [Reset]

errorMessage :: Text -> App ()
errorMessage msg = do
    setSGR stderr [SetColor Foreground Dull Red]
    lift $ hPutStrLn stderr msg
    setSGR stderr [Reset]
