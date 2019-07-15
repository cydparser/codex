{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Main.Config.Codex4 where

import           Data.Yaml
import           GHC.Generics

import qualified Codex as New

data Codex = Codex
  { currentProjectIncluded :: Bool
  , hackagePath            :: FilePath
  , stackOpts              :: String
  , tagsCmd                :: String
  , tagsFileHeader         :: Bool
  , tagsFileSorted         :: Bool
  , tagsFileName           :: FilePath
  } deriving (Generic)

instance FromJSON Codex

migrateWarn :: IO ()
migrateWarn = pure ()

migrate :: Codex -> New.Codex
migrate Codex{..} = New.Codex
  { builder     = Nothing
  , projectRoot = Nothing
  , ..
  }
