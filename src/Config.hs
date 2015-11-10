-- | This module pulls out a few values I'm more likely to want to configure
-- per-project.  They are passed to 'FixImports.runMain', so you can write
-- your own Main.hs that passes its own Config.
--
-- TODO dyre does this sort of thing
module Config where
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Language.Haskell.Exts.Annotated as Haskell
import qualified Language.Haskell.Exts.Extension as Extension
import qualified System.FilePath as FilePath

import qualified Index
import qualified Types
import qualified Util


data Config = Config {
    -- | Additional directories to search for local modules.  Taken from the
    -- -i flag and 'include' config line.
    configIncludes :: [FilePath]
    -- | These language extensions are enabled by default.
    , configLanguage :: [Extension.Extension]
    -- | Format the import block.
    , configShowImports :: [Types.ImportLine] -> String
    -- | Often multiple modules from the package index will match
    -- a qualification.  Apply some heuristics to pick the most likely one.
    , configPickModule :: FilePath -> [(Maybe Index.Package, Types.ModuleName)]
        -> Maybe (Maybe Index.Package, Types.ModuleName)
    }

empty :: Config
empty = Config
    { configIncludes = []
    , configLanguage = []
    , configShowImports = formatGroups $ ImportOrder []
    , configPickModule = pickModule defaultPriorities
    }

data Priorities = Priorities {
    -- | Place these packages either first or last in priority.
    prioPackage :: ([Index.Package], [Index.Package])
    -- | Place these modules either first or last in priority.
    , prioModule :: ([Types.ModuleName], [Types.ModuleName])
    } deriving (Show)

-- | Sort order for local modules.
newtype ImportOrder = ImportOrder [Types.ModuleName]
    deriving (Show)

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
    ([], ["haskell98", "ghc", "Cabal"])
    (map Types.ModuleName [], map Types.ModuleName ["GHC"])

-- * pick candidates

-- | Prefer local modules that share prefix with the module path, then prefer
-- local modules to ones from packages, then prefer modules from the packages
-- in packagePriority.  If all else is equal alphabetize so at least the
-- order is predictable.
pickModule :: Priorities -> FilePath
    -> [(Maybe Index.Package, Types.ModuleName)]
    -> Maybe (Maybe Index.Package, Types.ModuleName)
pickModule prios modulePath candidates =
    Util.head $ Util.sortOn (uncurry (prioritize prios modulePath)) $
        -- Don't pick myself!
        filter ((/= Types.pathToModule modulePath) . snd) candidates

prioritize :: Priorities -> FilePath -> Maybe String -> Types.ModuleName
    -> ((Int, Int), (Int, Int), String)
prioritize prios modulePath mbPackage mod =
    ( packagePrio (prioPackage prios) mbPackage
    , (modulePrio (prioModule prios) mod, dots mod)
    , Types.moduleName mod
    )
    where
    packagePrio _ Nothing = (localPrio modulePath mod, 0)
    packagePrio (high, low) (Just pack) = (1, searchPrio high low pack)
    modulePrio (high, low) =
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

-- | Print out the imports with spacing how I like it.
formatImports :: [Types.ImportLine] -> String
formatImports imports = unlines $
    map showImport (sort package) ++ [""] ++ map showImport (sort local)
    where
    sort = Util.sortOn Types.importModule
    (local, package) = List.partition Types.importIsLocal imports

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
formatGroups :: ImportOrder -> [Types.ImportLine] -> String
formatGroups (ImportOrder order) imports =
    unlines $ joinGroups
        [ showGroups (group (Util.sortOn packagePrio package))
        , showGroups (group (Util.sortOn localPrio local))
        ]
    where
    packagePrio imp = (fromEnum (name imp /= prelude), name imp,
        qualifiedPrio imp)
    localPrio imp = (listPriority (topModule imp) (map Types.moduleName order),
        name imp, qualifiedPrio imp)
    qualifiedPrio imp = fromEnum (not (qualifiedImport imp))
    name = Types.importModule
    (local, package) = List.partition Types.importIsLocal imports
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

listPriority :: (Eq a) => a -> [a] -> (Int, Maybe Int)
listPriority x xs = case List.elemIndex x xs of
    Nothing -> (1, Nothing)
    Just k -> (0, Just k)

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

{-
-- t0 = map localPrio imports -- formatGroups priorities (map mkImport imports)
t0 = formatGroups priorities imports
    where
    imports = map mkImport
        [ ("Data.List", True, Just "List", False)
        , ("Prelude", False, Nothing, False)
        , ("Prelude", True, Nothing, False)
        , local "A.B"
        , local "A.C"
        , local "C.A"
        , local "B.A"
        , local "B.B"
        , local "B.C"
        , local "B.D"
        ]
    local name = (name, True, Nothing, True)
    mkImport (name, qualified, importAs, local) = Types.ImportLine decl [] local
        where
        decl = Haskell.ImportDecl empty (Haskell.ModuleName empty name)
            qualified False Nothing (fmap (Haskell.ModuleName empty) importAs)
            Nothing
    empty = Haskell.SrcSpanInfo (Haskell.SrcSpan "" 0 0 0 0) []

    priorities = ["A", "B"]
    localPrio imp = (listPriority (topModule imp) priorities,
        name imp, fromEnum (qualifiedImport imp))
    topModule = takeWhile (/='.') . Types.moduleName . Types.importModule
    name = Types.importModule
-}
