{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmt.IO.Input
    ( loop
    ) where

import ClassyPrelude

import System.Console.Terminal.Size (size, width)

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

multiLine :: [Text] -> IO [Text]
multiLine input = do
    value <- prompt ">"
    if null value
        then pure input
        else multiLine $ input ++ [value]

output :: Part -> IO (Name, Text)
output (Part name Line) = do
    putName name
    val <- prompt ">"
    if null val
        then output (Part name Line)
        else pure (name, val)
output (Part name (Options opts)) = do
    putName name
    let opts' = zip [1 ..] opts
    let long = intercalate "   " $ listItem <$> opts'
    maxLength <- getWidth
    if length long < maxLength
        then putStrLn long
        else sequence_ $ putStrLn . listItem <$> opts'
    chosen <- prompt ">"
    case find ((== chosen) . tshow . fst) opts' of
        Nothing     -> output (Part name (Options opts))
        Just (_, o) -> pure (name, o)
output (Part name Lines) = do
    putName name
    val <- unlines <$> multiLine []
    pure (name, val)

loop :: Config -> IO [(Name, Text)]
loop (Config parts _) = sequence $ output <$> parts
