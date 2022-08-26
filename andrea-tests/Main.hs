{- cabal:
build-depends: base ^>= 4.14
             , bytestring
             , containers
             , directory
             , filepath
             , Cabal                >=3.8 && <3.9
             , cabal-install        >=3.8 && <3.9
             , cabal-install-solver >=3.8 && <3.9
             , pretty-simple
-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Main where

import Distribution.Client.DistDirLayout
import Distribution.Client.HttpUtils
import Distribution.Client.IndexUtils
import Distribution.Client.InstallPlan
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectPlanOutput (writePlanExternalRepresentation)
import Distribution.Client.ProjectPlanning
import Distribution.Client.Types.SourcePackageDb
import Distribution.Simple.Compiler
import Distribution.Simple.Configure
import Distribution.Simple.Flag
import Distribution.Simple.Program.Db
import Distribution.Solver.Types.PkgConfigDb
import Distribution.Verbosity
import System.Environment
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  [path] <- getArgs

  let verbosity = verbose
  let distDirLayout = defaultDistDirLayout (ProjectRootImplicit path) Nothing

  httpTransport <- configureTransport verbosity mempty Nothing

  (projectConfig, localPackages) <-
    rebuildProjectConfig
      verbosity
      httpTransport
      distDirLayout
      mempty

  putStrLn "-- projectConfig --"
  pPrint projectConfig

  putStrLn "-- localPackages --"
  pPrint localPackages

  let cabalDirLayout = defaultCabalDirLayout "/tmp/cabal-dir"

  -- Two variants of the install plan are returned: with and without
  -- packages from the store. That is, the "improved" plan where source
  -- packages are replaced by pre-existing installed packages from the
  -- store (when their ids match), and also the original elaborated plan
  -- which uses primarily source packages.
  (improvedPlan, elaboratedPlan, esc, tis, at) <-
    rebuildInstallPlan verbosity distDirLayout cabalDirLayout projectConfig localPackages

  putStrLn $ unlines ["-- imporovedPlan -- ", showInstallPlan improvedPlan]
  putStrLn $ unlines ["-- elaboratedPlan --", showInstallPlan elaboratedPlan]
  putStrLn "-- elaborated shared config --"
  pPrint esc
  putStrLn "-- total index state --"
  pPrint tis
  putStrLn "-- active repos --"
  pPrint at

  -- writes plan.json in distProjectCacheFile distDirLayout
  writePlanExternalRepresentation distDirLayout elaboratedPlan esc

main2 :: IO ()
main2 = do
  [path] <- getArgs

  let verbosity = verbose
  let distDirLayout = defaultDistDirLayout (ProjectRootImplicit path) Nothing

  httpTransport <- configureTransport verbosity mempty Nothing

  (projectConfig, localPackages) <-
    rebuildProjectConfig
      verbosity
      httpTransport
      distDirLayout
      mempty

  let ProjectConfig {projectConfigShared} = projectConfig

  putStrLn "-- projectConfig --"
  -- pPrint projectConfig

  putStrLn "-- localPackages --"
  -- pPrint localPackages

  let cabalDirLayout = defaultCabalDirLayout "/tmp/cabal-dir"

  (compiler, platform, progdb) <-
    let hcFlavor = flagToMaybe (projectConfigHcFlavor projectConfigShared)
        hcPath = flagToMaybe (projectConfigHcPath projectConfigShared)
        hcPkg = flagToMaybe (projectConfigHcPkg projectConfigShared)
     in configCompilerEx hcFlavor hcPath hcPkg defaultProgramDb verbosity

  let solverSettings = resolveSolverSettings projectConfig

  (sourcePkgDb, totalIndexState, activeRepos) <- projectConfigWithSolverRepoContext
    verbosity
    projectConfigShared
    (projectConfigBuildOnly projectConfig)
    $ \repoctx ->
      getSourcePackagesAtIndexState
        verbosity
        repoctx
        (solverSettingIndexState solverSettings)
        (solverSettingActiveRepos solverSettings)

  pkgConfigDB <- readPkgConfigDb verbosity progdb

  installedPackages <-
    let corePackagesDb = applyPackageDbFlags [GlobalPackageDB] (projectConfigPackageDBs projectConfigShared)
     in Distribution.Simple.Configure.getInstalledPackages verbosity compiler corePackagesDb progdb

  -- into planPackages now

  -- planPackages calls
  --    resolveDependencies
  --      platform (compilerInfo comp)
  --      pkgConfigDB solver
  --      resolverParams
  --

  -- end

  putStrLn "it works"

deriving instance Show SourcePackageDb

-- | Append the given package databases to an existing PackageDBStack.
-- A @Nothing@ entry will clear everything before it.
applyPackageDbFlags :: PackageDBStack -> [Maybe PackageDB] -> PackageDBStack
applyPackageDbFlags dbs' [] = dbs'
applyPackageDbFlags _ (Nothing : dbs) = applyPackageDbFlags [] dbs
applyPackageDbFlags dbs' (Just db : dbs) = applyPackageDbFlags (dbs' ++ [db]) dbs
