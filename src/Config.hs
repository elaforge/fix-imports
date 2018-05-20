-- | This module pulls out a few values I'm more likely to want to configure
-- per-project.  They are passed to 'FixImports.runMain', so you can write
-- your own Main.hs that passes its own Config.
--
-- TODO dyre does this sort of thing
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
-- Use partial signatures for sort keys, I don't care what they are as long as
-- they are in Ord.
module Config where
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Debug.Trace as Trace
import qualified Language.Haskell.Exts as Haskell
import qualified Language.Haskell.Exts.Extension as Extension
import qualified System.FilePath as FilePath
import qualified Index
import qualified Types
import qualified Util


data Config = Config {
    -- | Additional directories to search for local modules.  Taken from the
    -- -i flag and 'include' config line.
    includes :: [FilePath]
    -- | These language extensions are enabled by default.
    , language :: [Extension.Extension]
    -- | Format the import block.
    , showImports :: [Types.ImportLine] -> String
    -- | Often multiple modules from the package index will match
    -- a qualification.  Apply some heuristics to pick the most likely one.
    , pickModule :: FilePath -> [(Maybe Index.Package, Types.ModuleName)]
        -> Maybe (Maybe Index.Package, Types.ModuleName)
    }

empty :: Config
empty = Config
    { includes = []
    , language = []
    , showImports = formatGroups $ Priority
        { high = []
        , low = []
        }
    , pickModule = makePickModule defaultPriorities
    }

data Priorities = Priorities {
    -- | Place these packages either first or last in priority.
    prioPackage :: Priority Index.Package
    -- | Place these modules either first or last in priority.
    , prioModule :: Priority Types.ModuleName
    } deriving (Show)

data Priority a = Priority { high :: [a], low :: [a] } deriving (Show, Functor)

parseLanguage :: [String] -> ([String], [Extension.Extension])
parseLanguage = Either.partitionEithers . map parse
    where
    parse w = case Extension.parseExtension w of
        Extension.UnknownExtension _ -> Left w
        ext -> Right ext

defaultPriorities :: Priorities
defaultPriorities = Priorities
    -- Make some common packages low priority so their exports don't get
    -- chosen over what you probably wanted:
    -- haskell98 has obsolete toplevel module names.
    -- ghc exports tons of toplevel modules that you probably don't want.
    -- Cabal is probably mostly used in Setup.hs and exports Distribution.Text.
    { prioPackage = Priority
        { high = []
        , low = ["haskell98", "ghc", "Cabal"]
        }
    , prioModule = Priority
        { high = map Types.ModuleName ["Prelude"]
        , low = map Types.ModuleName ["GHC"]
        }
    }

-- * pick candidates

-- | The order of priority is:
--
-- - high or low in 'prioModule'
-- - local modules that share prefix with the module path
-- - local modules to ones from packages
-- - package modules high or low in 'prioPackage'
-- - If all else is equal alphabetize so at least the order is predictable.
makePickModule :: Priorities -> FilePath
    -> [(Maybe Index.Package, Types.ModuleName)]
    -> Maybe (Maybe Index.Package, Types.ModuleName)
makePickModule prios modulePath candidates =
    Util.minimumOn (uncurry (pickModuleKey prios modulePath)) $
        -- Don't pick myself!
        filter ((/= Types.pathToModule modulePath) . snd) candidates

pickModuleKey :: Priorities -> FilePath -> Maybe String -> Types.ModuleName
    -> _
pickModuleKey prios modulePath mbPackage mod =
    ( (modulePrio (prioModule prios) mod, dots mod)
    , packagePrio (prioPackage prios) mbPackage
    , Types.moduleName mod
    )
    where
    packagePrio _ Nothing = (localPrio modulePath mod, (0, Nothing))
    packagePrio prio (Just pkg) = (1, prioKey prefixIndex prio pkg)
    modulePrio prio = prioKey prefixIndex (Types.moduleName <$> prio)
        . Types.moduleName
    dots = length . filter (=='.') . Types.moduleName

-- | Lower numbers for modules that share more prefix with the module's path.
-- A/B/Z.hs vs A.B.C -> -2
-- A/Z.hs vs B -> 0
localPrio :: FilePath -> Types.ModuleName -> Int
localPrio modulePath mod = negate $ length $ takeWhile id $ zipWith (==)
    (Util.split "/" (Types.moduleToPath mod))
    (Util.split "/" (FilePath.takeDirectory modulePath))


-- * format imports

-- | Print out the imports with spacing how I like it.
formatImports :: [Types.ImportLine] -> String
formatImports imports = unlines $
    map showImport (sort package) ++ [""] ++ map showImport (sort local)
    where
    sort = Util.sortOn Types.importModule
    (local, package) =
        List.partition ((==Types.Local) . Types.importSource) imports

-- | Format import list.  Imports are alphabetized and grouped into sections
-- based on the top level module name (before the first dot).  Sections that
-- are too small are grouped with the section below them.
--
-- The local imports are sorted and grouped separately from the package
-- imports.  Rather than being alphabetical, they are sorted in a per-project
-- order that should be general-to-specific.
--
-- An unqualified import will follow a qualified one.  The Prelude, if
-- imported, always goes first.

-- formatGroups :: Priority Types.ModuleName -> [Types.ImportLine] -> String
-- formatGroups order imports =
--     unlines $ joinGroups
--         [ showGroups (group (Util.sortOn packagePrio package))
--         , showGroups (group (Util.sortOn localPrio local))
--         ]
--     where
--     packagePrio imp =
--         ( name imp /= prelude
--         , name imp
--         , qualifiedPrio imp
--         )
--     localPrio imp =
--         ( localPriority order (topModule imp)
--         , name imp
--         , qualifiedPrio imp
--         )
--     qualifiedPrio = not . qualifiedImport
--     name = Types.importModule
--     (local, package) =
--         List.partition ((==Types.Local) . Types.importSource) imports
--     group = collapse . Util.groupOn topModule
--     topModule = takeWhile (/='.') . Types.moduleName . Types.importModule
--     collapse [] = []
--     collapse (x:xs)
--         | length x <= 2 = case collapse xs of
--             [] -> [x]
--             y : ys -> (x ++ y) : ys
--         | otherwise = x : collapse xs
--     showGroups = List.intercalate [""] . map (map showImport)
--     joinGroups = List.intercalate [""] . filter (not . null)
--     prelude = Types.ModuleName "Prelude"

formatGroups :: Priority Types.ModuleName -> [Types.ImportLine] -> String
formatGroups prio =
    unlines . showGroups . group . Util.sortOn (importSortKey (Trace.traceShowId prio))
    where
    group = collapse . Util.groupOn topModule
    topModule = takeWhile (/='.') . Types.moduleName . Types.importModule
    collapse [] = []
    collapse (x:xs)
        | length x <= 2 = case collapse xs of
            [] -> [x]
            y : ys -> (x ++ y) : ys
        | otherwise = x : collapse xs
    showGroups = List.intercalate [""] . map (map showImport)

-- | The priority order is:
--
-- - Package imports go beore Local imports.  Local imports use the longest
-- match in 'Priority'.  The rationale is that ordering rules are
-- project-specific, so they don't apply to imports from external projects.
--
-- - The longest match in Priority Types.ModuleName.
--
-- - Import name.
--
-- - Qualified before unqualified.
importSortKey :: Priority Types.ModuleName -> Types.ImportLine -> _
importSortKey prio imp = Trace.traceShowId
    ( case Types.importSource imp of
        Types.Package -> Nothing
        Types.Local -> Just $
            prioKey longestPrefixIndex prio (Types.importModule imp)
    , Types.importModule imp
    , not $ qualifiedImport imp
    )

prioKey :: (b -> [a] -> Maybe c) -> Priority a -> b -> (Int, Maybe c)
prioKey match prio mod = case match mod (high prio) of
    Just i -> (-1, Just i)
    Nothing -> case match mod (low prio) of
        Nothing -> (0, Nothing)
        Just i -> (1, Just i)

longestPrefixIndex :: Types.ModuleName -> [Types.ModuleName] -> Maybe Int
longestPrefixIndex mod mods =
    snd <$> Util.maximumOn fst
        [ (commonPrefix (Types.moduleName mod) (Types.moduleName m), i)
        | (i, m) <- zip [0..] mods
        ]

prefixIndex :: String -> [String] -> Maybe Int
prefixIndex str strs = List.findIndex (`List.isPrefixOf` str) strs

commonPrefix :: Eq a => [a] -> [a] -> Int
commonPrefix xs ys = length . takeWhile (uncurry (==)) $ zip xs ys

-- | Modules whose top level element is in 'importFirst' go first, ones in
-- 'importLast' go last, and the rest go in the middle.
localPriority :: Priority Types.ModuleName -> String -> (Int, Maybe Int)
localPriority order importTop = case List.elemIndex importTop firsts of
    Just k -> (-1, Just k)
    Nothing -> case List.elemIndex importTop lasts of
        Nothing -> (0, Nothing)
        Just k -> (1, Just k)
    where
    firsts = map Types.moduleName (high order)
    lasts = map Types.moduleName (low order)

qualifiedImport :: Types.ImportLine -> Bool
qualifiedImport = Haskell.importQualified . Types.importDecl

showImport :: Types.ImportLine -> String
showImport (Types.ImportLine imp cmts _) =
    above ++ importLine ++ (if null right then "" else ' ' : right)
    where
    above = concat [cmt ++ "\n" | Types.Comment Types.CmtAbove cmt <- cmts]
    importLine = Haskell.prettyPrintStyleMode style mode imp
    right = Util.join "\n" [cmt | Types.Comment Types.CmtRight cmt <- cmts]
    style = Haskell.style
        { Haskell.lineLength = 80
        , Haskell.ribbonsPerLine = 1
        }
    mode = Haskell.defaultMode
