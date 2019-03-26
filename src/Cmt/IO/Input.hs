{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Cmt.IO.Input
    ( loop
    ) where

import ClassyPrelude

import Data.Map.Strict              (Map)
import System.Console.Terminal.Size (size, width)

import Cmt.IO.Git         (changed)
import Cmt.Parser.Options (parse)
import Cmt.Types.Config

type Choice = Map Int Text

prompt :: Text -> IO Text
prompt s = do
    putStr $ s <> " "
    hFlush stdout
    getLine

getWidth :: IO Int
getWidth = maybe 0 width <$> size

putName :: Text -> IO ()
putName name = putStrLn $ "\n" <> name <> ":"

listItem :: Int -> Text -> Text
listItem n o = tshow n <> ") " <> o

displayOptions :: Choice -> IO ()
displayOptions opts = do
    let parts = mapWithKey listItem opts
    let long = intercalate "   " parts
    maxLength <- getWidth
    if length long < maxLength
        then putStrLn long
        else sequence_ $ putStrLn <$> parts

choice :: Choice -> Int -> Maybe Text
choice opts chosen = lookup chosen opts

choiceGen :: [Text] -> Choice
choiceGen ts = mapFromList $ zip [1 ..] ts

options :: [Text] -> IO Text
options opts = do
    let opts' = choiceGen opts
    displayOptions opts'
    chosen <- parse <$> prompt ">"
    case chosen of
        Nothing -> options opts
        Just chosen' -> do
            let picked = catMaybes $ choice opts' <$> chosen'
            if null picked
                then options opts
                else pure $ intercalate ", " picked

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

output :: Part -> IO Output
output (Part name Line)           = putName name >> (,) name <$> line
output (Part name (Options opts)) = putName name >> (,) name <$> options opts
output (Part name Lines)          = putName name >> (,) name <$> (unlines <$> multiLine [])
output (Part name Changed)        = putName name >> (,) name <$> (options =<< changed)

loop :: Config -> IO Outputs
loop (Config parts _) = mapFromList <$> traverse output parts
