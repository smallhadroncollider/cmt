module Cmt.Types.App
    ( App
    , Settings(Settings)
    , defaultSettings
    , settingsDryRun
    , settingsColourize
    ) where

import ClassyPrelude

data Settings = Settings
    { settingsDryRun    :: Bool
    , settingsColourize :: Bool
    } deriving (Eq, Show)

type App a = ReaderT Settings IO a

defaultSettings :: Settings
defaultSettings = Settings {settingsDryRun = False, settingsColourize = True}
