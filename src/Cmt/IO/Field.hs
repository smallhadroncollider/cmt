{-# LANGUAGE NoImplicitPrelude #-}

module Cmt.IO.Field
    ( field
    ) where

import ClassyPrelude
import System.Console.ANSI

field :: Int -> Text -> IO ()
field limit text = do
    clearLine
    setCursorColumn 0
    putStr text
    hFlush stdout
    x <- getChar
    let new =
            if x == '\DEL'
                then dropEnd 1 text
                else text <> singleton x
    if length new < limit
        then field limit new
        else field limit text
