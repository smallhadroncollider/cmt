{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmt.IO.CLI where

import ClassyPrelude

import Data.Text.IO        (hPutStrLn)
import System.Console.ANSI (Color (Blue, Magenta, Red, Yellow), ColorIntensity (Dull),
                            ConsoleLayer (Foreground), SGR (Reset, SetColor), hSetSGR)

blank :: IO ()
blank = putStrLn ""

message :: Text -> IO ()
message msg = do
    hSetSGR stdout [SetColor Foreground Dull Blue]
    hPutStrLn stdout msg
    hSetSGR stdout [Reset]

mehssage :: Text -> IO ()
mehssage msg = do
    hSetSGR stdout [SetColor Foreground Dull Yellow]
    hPutStrLn stdout msg
    hSetSGR stdout [Reset]

header :: Text -> IO ()
header msg = do
    hSetSGR stdout [SetColor Foreground Dull Magenta]
    hPutStrLn stdout $ "*** " ++ msg ++ " ***"
    hSetSGR stdout [Reset]

errorMessage :: Text -> IO ()
errorMessage msg = do
    hSetSGR stderr [SetColor Foreground Dull Red]
    hPutStrLn stderr msg
    hSetSGR stderr [Reset]
