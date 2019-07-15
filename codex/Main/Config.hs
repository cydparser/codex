{-# LANGUAGE CPP #-}
module Main.Config where

import Control.Exception (catch)
import Control.Monad.Trans.Except
import Data.Yaml
import System.Directory
import System.FilePath

import Codex

import qualified Main.Config.Codex0 as C0
import qualified Main.Config.Codex1 as C1
import qualified Main.Config.Codex2 as C2
import qualified Main.Config.Codex3 as C3
import qualified Main.Config.Codex4 as C4

data ConfigState = Ready | TaggerNotFound

getConfigPath :: IO FilePath
getConfigPath = do
  homedir <- getHomeDirectory
  return $ homedir </> ".codex"

checkConfig :: Codex -> IO ConfigState
checkConfig cx = do
  taggerExe <- findExecutable tagger
  return $ case taggerExe of
    Just _    -> Ready
    _         -> TaggerNotFound
  where
    tagger = head $ words (tagsCmd cx)

loadConfig :: IO Codex
loadConfig = decodeConfig >>= maybe defaultConfig return where
  defaultConfig = do
    let cx = Codex True (dropFileName hp) defaultStackOpts (taggerCmd Hasktags) True True defaultTagsFileName
    encodeConfig cx
    return cx

encodeConfig :: Codex -> IO ()
encodeConfig cx = do
  path <- getConfigPath
  encodeFile path cx

decodeConfig :: IO (Maybe CodexYaml)
decodeConfig = do
  path  <- getConfigPath
  runExceptT $ configOf path
    <|> reencodeConfigOf C4.migrate C4.migrateWarn path
    <|> reencodeConfigOf C3.migrate C3.migrateWarn path
    <|> reencodeConfigOf C2.migrate C2.migrateWarn path
    <|> reencodeConfigOf C1.migrate C1.migrateWarn path
    <|> reencodeConfigOf C0.migrate C0.migrateWarn path
  where
    warn :: IO () -> IO ()
    warn migrateWarn = do
      putStrLn "codex: *warning* your configuration has been migrated automatically!\n"
      migrateWarn
      putStrLn ""

    reencodeConfigOf migrate migrateWarn path = do
      cfg <- migrate <$> configOf path
      liftIO $ do
        encodeConfig cfg
        warn migrateWarn
      pure cfg

    configOf path = ExceptT $ first (:[]) <$> decodeFileEither path

finalizeConfig :: CodexYaml -> IO Codex
finalizeConfig cfg =
  fromMaybe Cabal <$> runMaybeT (maybeStack <|> maybeCabalV2)

  maybeStack = do
    guard' $ doesDirectoryExist ("." </> ".stack-work")
    guard' $ doesFileExist ("." </> "stack.yaml")

    liftIO $ readCreateProcessWithExitCode (shell "which stack") "" >>= \case
      (ExitSuccess, _, _) -> liftIO $ do
        let opts = stackOpts cfg
        rootPath <- readStackPath opts "stack-root"
        binPath  <- readStackPath opts "bin-path"
        path     <- getEnv "PATH"
        setEnv "PATH" $ concat [path, ":", binPath]
        pure Stack -- (rootPath </> "indices" </> "Hackage")
      _ -> mzero

  maybeCabalV2 = do
    guard' $ doesDirectoryExist ("." </> "dist-newstyle")
    pure CabalV2

  guard' mb = guard =<< liftIO mb
