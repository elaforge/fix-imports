-- | Per-project 'Config' and functions to interpret it.
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Config where
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text

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
    , importPriority :: Priority Types.ModuleName
    -- | Heuristics to pick the right module.
    , modulePriority :: Priorities
    } deriving (Eq, Show)

data Priorities = Priorities {
    -- | Place these packages either first or last in priority.
    prioPackage :: Priority Index.Package
    -- | Place these modules either first or last in priority.
    , prioModule :: Priority Types.ModuleName
    } deriving (Eq, Show)

data Priority a = Priority { high :: [a], low :: [a] }
    deriving (Eq, Show)

empty :: Config
empty = Config
    { includes = []
    , language = []
    , importPriority = Priority { high = [], low = [] }
    , modulePriority = defaultPriorities
    }

-- | Parse .fix-imports file.
parse :: Text.Text -> (Config, [String])
parse text = (config, errors)
    where
    commas = List.intercalate ", "
    errors =
        [ ".fix-imports has unrecognized fields: "
            ++ commas unknownFields | not (null unknownFields) ]
        ++ [ ".fix-imports has unknown language extensions: "
            ++ commas unknownLanguage | not (null unknownLanguage) ]
    config = empty
        { includes = get "include"
        , language = language
        , importPriority = Priority
            { high = getModules "import-order-first"
            , low = getModules "import-order-last"
            }
        , modulePriority = Priorities
            { prioPackage = Priority
                { high = get "prio-package-high"
                , low = get "prio-package-low"
                }
            , prioModule = Priority
                { high = getModules "prio-module-high"
                , low = getModules "prio-module-low"
                }
            }
        }
    (unknownLanguage, language) = parseLanguage (get "language")
    unknownFields = Map.keys fields List.\\ valid
    valid =
        [ "include"
        , "import-order-first", "import-order-last"
        , "prio-package-high", "prio-package-low"
        , "prio-module-high", "prio-module-low"
        , "language"
        ]
    fields = Map.fromList [(Text.unpack section, map Text.unpack words)
        | (section, words) <- Index.parseSections text]
    getModules = map Types.ModuleName . get
    get k = Map.findWithDefault [] k fields

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
        { high = map Types.ModuleName []
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
pickModule :: Priorities -> FilePath
    -> [(Maybe Index.Package, Types.ModuleName)]
    -> Maybe (Maybe Index.Package, Types.ModuleName)
pickModule prios modulePath candidates =
    Util.minimumOn (uncurry (prioritize prios modulePath)) $
        -- Don't pick myself!
        filter ((/= Types.pathToModule modulePath) . snd) candidates

prioritize :: Priorities -> FilePath -> Maybe String -> Types.ModuleName
    -> ((Int, Int), (Int, Int), String)
prioritize prios modulePath mbPackage mod =
    ( (modulePrio (prioModule prios) mod, dots mod)
    , packagePrio (prioPackage prios) mbPackage
    , Types.moduleName mod
    )
    where
    packagePrio _ Nothing = (localPrio modulePath mod, 0)
    packagePrio (Priority {high, low}) (Just pkg) =
        (1, searchPrio high low pkg)
    modulePrio (Priority {high, low}) =
        searchPrio (map Types.moduleName high) (map Types.moduleName low)
        . Types.moduleName
    dots = length . filter (=='.') . Types.moduleName

-- | Lower numbers for modules that share more prefix with the module's path.
-- A/B/Z.hs vs A.B.C -> -2
-- A/Z.hs vs B -> 0
localPrio :: FilePath -> Types.ModuleName -> Int
localPrio modulePath mod = negate $ length $ takeWhile id $ zipWith (==)
    (Util.split "/" (Types.moduleToPath mod))
    (Util.split "/" (FilePath.takeDirectory modulePath))

searchPrio :: [String] -> [String] -> String -> Int
searchPrio high low mod = case List.findIndex (`List.isPrefixOf` mod) high of
    Just n -> - length high + n
    Nothing -> maybe 0 (+1) (List.findIndex (`List.isPrefixOf` mod) low)


-- * format imports

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
formatGroups :: Priority Types.ModuleName -> [Types.ImportLine] -> String
formatGroups prio imports =
    unlines $ joinGroups
        [ showGroups (group (Util.sortOn packagePrio package))
        , showGroups (group (Util.sortOn localPrio local))
        ]
    where
    packagePrio imp =
        ( name imp /= prelude
        , name imp
        , qualifiedPrio imp
        )
    localPrio imp =
        ( localPriority prio (topModule imp)
        , name imp
        , qualifiedPrio imp
        )
    qualifiedPrio = not . qualifiedImport
    name = Types.importModule
    (local, package) =
        List.partition ((==Types.Local) . Types.importSource) imports
    group = collapse . Util.groupOn topModule
    topModule = takeWhile (/='.') . Types.moduleName . Types.importModule
    collapse [] = []
    collapse (x:xs)
        | length x <= 2 = case collapse xs of
            [] -> [x]
            y : ys -> (x ++ y) : ys
        | otherwise = x : collapse xs
    showGroups = List.intercalate [""] . map (map showImport)
    joinGroups = List.intercalate [""] . filter (not . null)
    prelude = Types.ModuleName "Prelude"

-- | Modules whose top level element is in 'importFirst' go first, ones in
-- 'importLast' go last, and the rest go in the middle.
localPriority :: Priority Types.ModuleName -> String -> (Int, Maybe Int)
localPriority prio importTop = case List.elemIndex importTop firsts of
    Just k -> (-1, Just k)
    Nothing -> case List.elemIndex importTop lasts of
        Nothing -> (0, Nothing)
        Just k -> (1, Just k)
    where
    firsts = map Types.moduleName (high prio)
    lasts = map Types.moduleName (low prio)

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
