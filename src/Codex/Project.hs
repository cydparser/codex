{-# LANGUAGE CPP #-}
module Codex.Project where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
import Data.Traversable (traverse)
#endif

import Control.Applicative ((<|>))
import Control.Exception (try, SomeException)
import Control.Monad (filterM)
import Data.Bool (bool)
import Data.Function
import Data.List (delete, isPrefixOf, union)
import Data.Maybe
import Distribution.InstalledPackageInfo
#if MIN_VERSION_hackage_db(2,0,0)
import Distribution.Hackage.DB (HackageDB, cabalFile, hackageTarball, readTarball)
#else
import Distribution.Hackage.DB (Hackage, readHackage')
#endif
import Distribution.Package
import Distribution.PackageDescription
#if MIN_VERSION_Cabal(2,2,0)
import Distribution.PackageDescription.Parsec
#else
import Distribution.PackageDescription.Parse
#endif
import Distribution.Sandbox.Utils (findSandbox)
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PackageIndex
import Distribution.Verbosity
import Distribution.Version
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath
import Text.Read (readMaybe)

import qualified Data.List as List
import qualified Data.Map as Map
#if !MIN_VERSION_hackage_db(2,0,0)
import qualified Data.Version as Base
#endif

import Codex.HackageDB
import Codex.Internal (Builder(..), stackListDependencies)

newtype Workspace = Workspace [WorkspaceProject]
  deriving (Eq, Show)

data WorkspaceProject = WorkspaceProject { workspaceProjectIdentifier :: PackageIdentifier, workspaceProjectPath :: FilePath }
  deriving (Eq, Show)

type ProjectDependencies = (Maybe PackageIdentifier, [PackageIdentifier], [WorkspaceProject])

identifier :: GenericPackageDescription -> PackageIdentifier
identifier = package . packageDescription

allDependencies :: GenericPackageDescription -> [Dependency]
allDependencies pd = List.filter (not . isCurrent) $ concat [lds, eds, tds, bds] where
  lds = condTreeConstraints =<< (maybeToList $ condLibrary pd)
  eds = (condTreeConstraints . snd) =<< condExecutables pd
  tds = (condTreeConstraints . snd) =<< condTestSuites pd
  bds = (condTreeConstraints . snd) =<< condBenchmarks pd
  isCurrent (Dependency n _) = n == (pkgName $ identifier pd)

findPackageDescription :: FilePath -> IO (Maybe GenericPackageDescription)
findPackageDescription root = do
  mpath <- findCabalFilePath root
  traverse (
#if MIN_VERSION_Cabal(2,2,0)
    readGenericPackageDescription
#else
    readPackageDescription
#endif
    silent) mpath

-- | Find a regular file ending with ".cabal" within a directory.
findCabalFilePath :: FilePath -> IO (Maybe FilePath)
findCabalFilePath path = do
  paths <- getDirectoryContents path
  case List.find ((&&) <$> dotCabal <*> visible) paths of
    Just p -> do
      let p' = path </> p
      bool Nothing (Just p') <$> doesFileExist p'
    Nothing -> pure Nothing
  where
    dotCabal = (".cabal" ==) . takeExtension
    visible  = not . List.isPrefixOf "."

resolveCurrentProjectDependencies :: Builder -> IO ProjectDependencies
resolveCurrentProjectDependencies bldr = do
  mps <- localPackages
  case mps of
    Just ps -> resolveLocalDependencies bldr ps
    Nothing -> do
      disableImplicitWorkspace <- isJust <$> lookupEnv "CODEX_DISABLE_WORKSPACE"
      ws <- case workspaceDir of
        Nothing  -> pure (Workspace [])
        Just dir -> getWorkspace dir
      resolveProjectDependencies bldr ws "."
  where
    localPackages = do
      mpath <-
        case bldr of
          Cabal   -> maybeCabalProject
          CabalV2 -> maybeCabalProject
          Stack   -> pure (Just ".")
      case mpath of
        Nothing   -> pure Nothing
        Just path -> Just <$> findLocalPackages 2 path

    maybeCabalProject = bool Nothing (Just ".") <$> doesFileExist "cabal.project"

-- | Resolve the dependencies of each local project package.
resolveLocalDependencies :: Codex -> [WorkspaceProject] -> IO ProjectDependencies
resolveLocalDependencies Codex{..} wps = do
  pids <- foldr mergeDependencies mempty <$> traverse resolve wps
  pure (Nothing, pids, wps)
  where
    resolve p@WorkspaceProject{workspaceProjectPath = packagePath} =
      let ws' = Workspace (delete p wps)
      in resolveProjectDependencies bldr ws' packagePath
    mergeDependencies (_, pids, _) pids' =
      pids `union` pids'

-- TODO Optimize
resolveProjectDependencies :: Builder -> Workspace -> FilePath -> IO ProjectDependencies
resolveProjectDependencies bldr ws root = do
  pd <- maybe (error "No cabal file found.") id <$> findPackageDescription root
  xs <- resolvePackageDependencies bldr root pd
  ys <- case bldr of
    Cabal -> resolveSandboxDependencies root
    _     -> pure []
  let zs   = resolveWorkspaceDependencies ws pd
  let wsds = List.filter (shouldOverride xs) $ List.nubBy (on (==) prjId) $ concat [ys, zs]
  let pjds = List.filter (\x -> (((unPackageName . pkgName) x) /= "rts") && (List.notElem (pkgName x) $ fmap prjId wsds)) xs
  return (Just (identifier pd), pjds, wsds)
  where
    shouldOverride xs (WorkspaceProject x _) =
      maybe True (\y -> pkgVersion x >= pkgVersion y) $ List.find (\y -> pkgName x == pkgName y) xs
    prjId = pkgName . workspaceProjectIdentifier

resolveInstalledDependencies :: Codex -> FilePath -> GenericPackageDescription -> IO (Either SomeException [PackageIdentifier])
resolveInstalledDependencies Codex{..} root pd = try $ do
  case builder of
    Cabal -> do
      lbi <- withCabal
      let ipkgs = installedPkgs lbi
          clbis = allComponentsInBuildOrder' lbi
          pkgs  = componentPackageDeps =<< clbis
          ys = (maybeToList . lookupInstalledPackageId ipkgs) =<< fmap fst pkgs
          xs = fmap sourcePackageId $ ys
      return xs where
        withCabal = getPersistBuildConfig $ root </> "dist"
    CabalV2 -> undefined -- XXX plan.json
    Stack ->
      filter (/= pid) <$> stackListDependencies stackOpts pname
  where
    pid = pd & packageDescription & package
    pname = pid & pkgName & unPackageName

allComponentsInBuildOrder' :: LocalBuildInfo -> [ComponentLocalBuildInfo]
allComponentsInBuildOrder' =
#if MIN_VERSION_Cabal(2,0,0)
  allComponentsInBuildOrder
#else
  fmap snd . allComponentsInBuildOrder
#endif

resolveHackageDependencies :: Hackage -> GenericPackageDescription -> [GenericPackageDescription]
resolveHackageDependencies db pd = maybeToList . resolveDependency db =<< allDependencies pd where
  resolveDependency _ (Dependency name versionRange) = do
    pdsByVersion <- lookupName name
    latest <- List.find (\x -> withinRange' x versionRange) $ List.reverse $ List.sort $ Map.keys pdsByVersion
    lookupVersion latest pdsByVersion
#if MIN_VERSION_hackage_db(2,0,0)
  lookupName name = Map.lookup name db
  lookupVersion latest pdsByVersion = cabalFile <$> Map.lookup latest pdsByVersion
#else
  lookupName name = Map.lookup (unPackageName name) db
  lookupVersion latest pdsByVersion = Map.lookup latest pdsByVersion
#endif

#if MIN_VERSION_hackage_db(2,0,0)
withinRange' :: Version -> VersionRange -> Bool
withinRange' = withinRange
#else
withinRange' :: Base.Version -> VersionRange -> Bool
withinRange' =
#if MIN_VERSION_Cabal(2,0,0)
  withinRange . mkVersion'
#else
  withinRange
#endif
#endif

resolvePackageDependencies :: Builder -> FilePath -> GenericPackageDescription -> IO [PackageIdentifier]
resolvePackageDependencies bldr root pd =
  either fallback return =<< resolveInstalledDependencies bldr root pd
  where
    fallback e = do
      -- XXX TODO: what if Stack
      putStrLn $ concat ["codex: ", show e]
      putStrLn "codex: *warning* falling back on dependency resolution using hackage"
      db <- readHackageTarball
      pure $ identifier <$> resolveHackageDependencies db pd

readHackageTarball :: IO Hackage
readHackageTarball = do
#if MIN_VERSION_hackage_db(2,0,0)
  readTarball Nothing =<< hackageTarball
#else
  tarball <- do
    -- Copied from hackage-db.
    cabalDir <- getAppUserDataDirectory "cabal"
    let htd = cabalDir </> "packages" </> "hackage.haskell.org"
    let idx00 = htd </> "00-index.tar"
        idx01 = htd </> "01-index.tar"

    have01 <- doesFileExist idx01
    if have01 then return idx01 else do
      have00 <- doesFileExist idx00
      if have00 then return idx00 else
        throwIO NoHackageTarballFound
  readHackage' tarball
#endif

resolveSandboxDependencies :: FilePath -> IO [WorkspaceProject]
resolveSandboxDependencies root =
  findSandbox root >>= maybe (return []) continue
 where
  continue cabalSandboxFolder = do
    fileExists  <- doesFileExist sourcesFile
    if fileExists then readSources else return []
   where
    sourcesFile = root </> cabalSandboxFolder </> "add-source-timestamps"
    readSources = do
      fileContent <- readFile sourcesFile
      xs <- traverse readWorkspaceProject $ projects fileContent
      return $ xs >>= maybeToList where
        projects :: String -> [FilePath]
        projects x = sources x >>= (\x' -> fst <$> snd x')
        sources :: String -> [(String, [(FilePath, Int)])]
        sources x = fromMaybe [] (readMaybe x)

resolveWorkspaceDependencies :: Workspace -> GenericPackageDescription -> [WorkspaceProject]
resolveWorkspaceDependencies (Workspace ws) pd = maybeToList . resolveDependency =<< allDependencies pd where
  resolveDependency (Dependency name versionRange) =
    List.find (\(WorkspaceProject (PackageIdentifier n v) _) -> n == name && withinRange v versionRange) ws

readWorkspaceProject :: FilePath -> IO (Maybe WorkspaceProject)
readWorkspaceProject path = do
  pd <- findPackageDescription path
  return $ fmap (\x -> WorkspaceProject (identifier x) path) pd

getWorkspace :: FilePath -> IO Workspace
getWorkspace root =
  Workspace <$> findLocalPackages 1 root

-- | Recursively find local packages in @root@, up to @depth@ layers deep. The
-- @root@ directory has a depth of 0.
findLocalPackages :: Int -> FilePath -> IO [WorkspaceProject]
findLocalPackages depth root =
  catMaybes <$> go depth root
  where
    go n path
      | n < 0 = pure []
      | otherwise =
          (:) <$> readWorkspaceProject path
              <*> fmap mconcat (traverse (go (n - 1)) =<< listDirectories path)
    listDirectories path = do
      paths <- getDirectoryContents =<< canonicalizePath path
      filterM doesDirectoryExist ((path </>) <$> filter visible paths)
    visible path =
      (not . isPrefixOf ".") path && path `notElem` ["dist", "dist-newstyle"]
