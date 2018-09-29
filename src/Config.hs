-- | Per-project 'Config' and functions to interpret it.
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-} -- sort keys only care about Ord
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Config where
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO

import qualified Language.Haskell.Exts as Haskell
import qualified Language.Haskell.Exts.Extension as Extension
import qualified System.FilePath as FilePath
import qualified System.IO as IO

import qualified Index
import qualified Types
import qualified Util


data Config = Config {
    -- | Additional directories to search for local modules.  Taken from the
    -- -i flag and 'include' config line.
    _includes :: [FilePath]
    -- | These language extensions are enabled by default.
    , _language :: [Extension.Extension]
    -- | Import sort order.  Used by 'formatGroups'.
    , _order :: Order
    -- | Heuristics to pick the right module.  Used by 'pickModule'.
    , _modulePriority :: Priorities
    , _debug :: Bool
    } deriving (Eq, Show)

data Order = Order {
    _importOrder :: Priority ModulePattern
    -- | Put unqualified import-all imports last.
    , _sortUnqualifiedLast :: Bool
    } deriving (Eq, Show)

data Priorities = Priorities {
    -- | Place these packages either first or last in priority.
    prioPackage :: Priority Index.Package
    -- | Place these modules either first or last in priority.
    , prioModule :: Priority Types.ModuleName
    } deriving (Eq, Show)

data Priority a = Priority { high :: [a], low :: [a] }
    deriving (Eq, Show)

-- | A simple pattern: @M.@ matches M and M.*.  Anything else matches exactly.
type ModulePattern = String

matchModule :: ModulePattern -> Types.ModuleName -> Bool
matchModule pattern (Types.ModuleName mod) = case Util.unsnoc pattern of
    Nothing -> False
    Just (parent, '.') -> parent == mod || pattern `List.isPrefixOf` mod
    _ -> pattern == mod

empty :: Config
empty = Config
    { _includes = []
    , _language = []
    , _order = Order
        { _importOrder = Priority { high = [], low = [] }
        , _sortUnqualifiedLast = False
        }
    , _modulePriority = defaultPriorities
    , _debug = False
    }

-- | Parse .fix-imports file.
parse :: Text -> (Config, [Text])
parse text = (config, errors)
    where
    commas = Text.intercalate ", "
    errors = map (".fix-imports: "<>) $ concat
        [ [ "unrecognized fields: " <> commas unknownFields
          | not (null unknownFields)
          ]
        , [ "unknown language extensions: "
            <> commas (map Text.pack unknownLanguage)
          | not (null unknownLanguage)
          ]
        ]
    config = empty
        { _includes = get "include"
        , _language = language
        , _order = Order
            { _importOrder = Priority
                { high = get "import-order-first"
                , low = get "import-order-last"
                }
            , _sortUnqualifiedLast = getBool "sort-unqualified-last"
            }
        , _modulePriority = Priorities
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
        , "language"
        , "import-order-first", "import-order-last"
        , "prio-package-high", "prio-package-low"
        , "prio-module-high", "prio-module-low"
        , "sort-unqualified-last"
        ]
    fields = Map.fromList
        [ (section, map Text.unpack words)
        | (section, words) <- Index.parseSections text
        ]
    getModules = map Types.ModuleName . get
    get k = Map.findWithDefault [] k fields
    getBool k = k `Map.member` fields

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

pickModule :: Priorities -> FilePath
    -> [(Maybe Index.Package, Types.ModuleName)]
    -> Maybe (Maybe Index.Package, Types.ModuleName)
pickModule prios modulePath candidates =
    Util.minimumOn (uncurry (prioritize prios modulePath)) $
        -- Don't pick myself!
        filter ((/= Types.pathToModule modulePath) . snd) candidates

-- | The order of priority is:
--
-- - high or low in 'prioModule'
-- - local modules that share prefix with the module path
-- - local modules to ones from packages
-- - package modules high or low in 'prioPackage'
-- - prefer with fewer dots, so System.IO over Data.Text.IO
-- - If all else is equal alphabetize so at least the order is predictable.
prioritize :: Priorities -> FilePath -> Maybe String -> Types.ModuleName -> _
prioritize prios modulePath mbPackage mod =
    ( modulePrio (prioModule prios) mod
    , localPrio mbPackage
    , packagePrio (prioPackage prios) mbPackage
    , length $ filter (=='.') $ Types.moduleName mod
    , Types.moduleName mod
    )
    where
    localPrio Nothing = Before $ localOrder modulePath mod
    localPrio (Just _) = After

    packagePrio _ Nothing = Nothing
    packagePrio (Priority {high, low}) (Just pkg) =
        Just $ searchPrio high low pkg
    modulePrio (Priority {high, low}) =
        searchPrio (map Types.moduleName high) (map Types.moduleName low)
        . Types.moduleName

-- | This is like Maybe, except that a present value will always sort before an
-- absent one.
data Before a = Before a | After deriving (Eq, Show)
instance Ord a => Ord (Before a) where
    compare After After = EQ
    compare (Before _) After = LT
    compare After (Before _) = GT
    compare (Before a) (Before b) = compare a b

-- | Lower numbers for modules that share more prefix with the module's path.
-- A/B/Z.hs vs A.B.C -> -2
-- A/Z.hs vs B -> 0
localOrder :: FilePath -> Types.ModuleName -> Int
localOrder modulePath mod = negate $ length $ takeWhile id $ zipWith (==)
    (Util.split "/" (Types.moduleToPath mod))
    (Util.split "/" (FilePath.takeDirectory modulePath))

searchPrio :: [String] -> [String] -> String -> Int
searchPrio high low mod = case List.findIndex (== mod) high of
    Just n -> - length high + n
    Nothing -> maybe 0 (+1) (List.findIndex (== mod) low)


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
formatGroups :: Order -> [Types.ImportLine] -> String
formatGroups order imports =
    unlines $ joinGroups
        [ showGroups (group (Util.sortOn packagePrio package))
        , showGroups (group (Util.sortOn localPrio local))
        , showGroups [Util.sortOn name unqualified]
        ]
    where
    packagePrio import_ =
        ( name import_ /= prelude
        , name import_
        , qualifiedPrio import_
        )
    localPrio import_ =
        ( localPriority (_importOrder order) (Types.importModule import_)
        , name import_
        , qualifiedPrio import_
        )
    qualifiedPrio = not . qualifiedImport
    name = Types.importModule
    (unqualified, local, package) = Util.partition2
        ((_sortUnqualifiedLast order &&) . isUnqualified . Types.importDecl)
        ((==Types.Local) . Types.importSource)
        imports
    -- (local, package) =
    --     List.partition ((==Types.Local) . Types.importSource) imports
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

isUnqualified :: Haskell.ImportDecl a -> Bool
isUnqualified imp = not (Haskell.importQualified imp)
    && Maybe.isNothing (Haskell.importSpecs imp)

-- | Modules whose top level element is in 'importFirst' go first, ones in
-- 'importLast' go last, and the rest go in the middle.
--
-- Like 'searchPrio' but for order.
localPriority :: Priority ModulePattern -> Types.ModuleName
    -> (Int, Maybe Int)
localPriority prio import_ =
    case List.findIndex (`matchModule` import_) firsts of
        Just k -> (-1, Just k)
        Nothing -> case List.findIndex (`matchModule` import_) lasts of
            Nothing -> (0, Nothing)
            Just k -> (1, Just k)
    where
    firsts = high prio
    lasts = low prio

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

-- * log

debug :: Config -> Text -> IO ()
debug config msg
    | _debug config = Text.IO.hPutStrLn IO.stderr msg
    | otherwise = return ()
