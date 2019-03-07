{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmt.IO.Input
    ( loop
    ) where

import ClassyPrelude

import System.Console.Terminal.Size (size, width)

import Cmt.IO.Git         (changed)
import Cmt.Parser.Options (parse)
import Cmt.Types.Config

prompt :: Text -> IO Text
prompt s = do
    putStr $ s <> " "
    hFlush stdout
    getLine

getWidth :: IO Int
getWidth = maybe 0 width <$> size

putName :: Text -> IO ()
putName name = putStrLn $ "\n" <> name <> ":"

listItem :: (Int, Text) -> Text
listItem (n, o) = tshow n <> ") " <> o

displayOptions :: [(Int, Text)] -> IO ()
displayOptions opts = do
    let long = intercalate "   " $ listItem <$> opts
    maxLength <- getWidth
    if length long < maxLength
        then putStrLn long
        else sequence_ $ putStrLn . listItem <$> opts

choice :: [(Int, Text)] -> Int -> Maybe Text
choice opts chosen = snd <$> find ((== chosen) . fst) opts

options :: [Text] -> IO Text
options opts = do
    let opts' = zip [1 ..] opts
    displayOptions opts'
    chosen <- parse <$> prompt ">"
    case chosen of
        Nothing      -> options opts
        Just chosen' -> pure . intercalate ", " . catMaybes $ choice opts' <$> chosen'

multiLine :: [Text] -> IO [Text]
multiLine input = do
    value <- prompt ">"
    if null value && not (null input)
        then pure input
        else multiLine $ input ++ [value]

line :: IO Text
line = do
    value <- prompt ">"
    if null value
        then line
        else pure value

output :: Part -> IO (Name, Text)
output (Part name Line)           = putName name >> (,) name <$> line
output (Part name (Options opts)) = putName name >> (,) name <$> options opts
output (Part name Lines)          = putName name >> (,) name <$> (unlines <$> multiLine [])
output (Part name Changed)        = putName name >> (,) name <$> (options =<< changed)

loop :: Config -> IO [(Name, Text)]
loop (Config parts _) = sequence $ output <$> parts
