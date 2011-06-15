-- | This module pulls out a few values I'm more likely to want to configure
-- per-project.  They are passed to 'FixImports.runMain', so you can write
-- your own Main.hs that passes its own Config.
--
-- TODO dyre does this sort of thing
module Config where
import qualified Data.List as List
import qualified Language.Haskell.Exts.Annotated as Haskell

import qualified Types
import qualified Util
import qualified Index


data Config = Config {
    -- | Format the import block.
    configShowImports :: [Types.ImportLine] -> String
    -- | See 'Index.Config'.
    , configIndex :: Index.Config
    }

defaultConfig :: [String] -> Config
defaultConfig localModules = Config (formatGroups localModules)
    (Index.Config packagePriority [])

packagePriority :: [String]
packagePriority = ["base", "containers", "directory", "mtl"]

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
formatGroups :: [String] -> [Types.ImportLine] -> String
formatGroups priorities imports =
    unlines $ joinGroups
        [ showGroups (group (Util.sortOn packagePrio package))
        , showGroups (group (Util.sortOn localPrio local))
        ]
    where
    packagePrio imp = (fromEnum (name imp /= prelude), name imp,
        qualifiedPrio imp)
    localPrio imp = (listPriority (topModule imp) priorities,
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
    importLine = Haskell.prettyPrint imp
    right = Util.join "\n" [cmt | Types.Comment Types.CmtRight cmt <- cmts]

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
