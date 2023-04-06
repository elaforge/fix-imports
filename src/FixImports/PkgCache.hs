{-# LANGUAGE OverloadedStrings #-}
module FixImports.PkgCache (
    load, UnitId, PkgName, ModuleName
) where
import           Data.Maybe (mapMaybe)
import           Data.Set (Set)
import           Data.Text (Text)
import qualified Data.Text as Text
import           System.FilePath ((</>))
import qualified Data.Set as Set

import qualified Distribution.InstalledPackageInfo as IPI
import qualified Distribution.ModuleName as ModuleName
import qualified Distribution.Types.PackageId as PackageId
import qualified Distribution.Types.PackageName as PackageName
import qualified Distribution.Types.UnitId as UnitId
import qualified GHC.Unit.Database as Database


-- t_pkgs = Set.fromList . map fst <$> loadCache cache
-- t_load = load (Set.fromList ["base-4.16.4.0"])
--     [cache, global]
-- cache = "/Users/elaforge/.cabal/store/ghc-9.2.5/package.db"
-- global = "/Users/elaforge/.ghcup/ghc/9.2.5/lib/ghc-9.2.5/lib/package.conf.d"

type UnitId = Text
type PkgName = Text
type ModuleName = String

load :: Set UnitId -> [FilePath] -> IO [(PkgName, [ModuleName])]
load wanted pkgDbs = do
    pkgs <- concat <$> mapM (\dir -> loadCache (dir </> "package.cache"))
        pkgDbs
    pure $ map snd $ filter ((`Set.member` wanted) . fst) pkgs

loadCache :: FilePath -> IO [(UnitId, (PkgName, [ModuleName]))]
loadCache cachePath = do
    (pkgs, _) <- Database.readPackageDbForGhcPkg cachePath
        Database.DbOpenReadOnly
    pure $ mapMaybe extract pkgs

extract :: IPI.InstalledPackageInfo -> Maybe (UnitId, (PkgName, [ModuleName]))
extract pkg
    | not (IPI.exposed pkg) = Nothing
    | otherwise = Just
        ( Text.pack $ UnitId.unUnitId $ IPI.installedUnitId pkg
        , ( pkgName (IPI.sourcePackageId pkg)
          , map (modString . IPI.exposedName) $ IPI.exposedModules pkg
          )
        )
    where
    modString = map (\c -> if c == '/' then '.' else c) . ModuleName.toFilePath
    pkgName = Text.pack . PackageName.unPackageName . PackageId.pkgName
