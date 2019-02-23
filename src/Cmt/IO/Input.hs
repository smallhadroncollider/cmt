{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cmt.IO.Input
    ( loop
    ) where

import ClassyPrelude

import Cmt.Types.Config

listItem :: (Int, Text) -> IO ()
listItem (n, o) = putStrLn $ tshow n <> ") " <> o

multiLine :: [Text] -> IO [Text]
multiLine input = do
    value <- getLine
    if null value
        then pure input
        else multiLine $ input ++ [value]

output :: Part -> IO (Name, Text)
output (Part name Line) = do
    putStrLn $ name <> ":"
    val <- getLine
    if null val
        then output (Part name Line)
        else pure (name, val)
output (Part name (Options opts)) = do
    let opts' = zip [1 ..] opts
    putStrLn $ name <> ":"
    sequence_ $ listItem <$> opts'
    chosen <- getLine
    case find ((== chosen) . tshow . fst) opts' of
        Nothing     -> output (Part name (Options opts))
        Just (_, o) -> pure (name, o)
output (Part name Lines) = do
    putStrLn $ name <> ":"
    val <- unlines <$> multiLine []
    pure (name, val)

loop :: Config -> IO [(Name, Text)]
loop (Config parts _) = sequence $ output <$> parts
