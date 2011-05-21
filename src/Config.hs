module Config where
import qualified Data.List as List
import qualified Language.Haskell.Exts.Annotated as Haskell

import qualified Types
import qualified Util
import qualified Index


data Config = Config {
    -- | Format the impor block.
    configShowImports :: [Types.ImportLine] -> String
    -- | See 'Index.Config'.
    , configIndex :: Index.Config
    }

defaultConfig :: [String] -> Config
defaultConfig localModules = Config (formatGroups localModules)
    (Index.Config packagePriority [])

packagePriority :: [String]
packagePriority = ["base", "containers", "mtl"]

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
formatGroups :: [String] -> [Types.ImportLine] -> String
formatGroups priorities imports =
    unlines $ joinGroups
        [ showGroups (group (Util.sortOn Types.importModule package))
        , showGroups (group (Util.sortOn priority local))
        ]
    where
    priority imp =
        (List.elemIndex (topModule imp) priorities, Types.importModule imp)
    (local, package) = List.partition Types.importIsLocal imports
    group = collapse . Util.groupOn topModule
    topModule = takeWhile (/='.') . Types.moduleName . Types.importModule
    collapse [] = []
    collapse (x:xs)
        | length x <= 2 = case collapse xs of
            [] -> [x]
            y : ys -> (x ++ y) : ys
        | otherwise = x : collapse xs
    showGroups = concat . List.intersperse [""] . map (map showImport)
    joinGroups = concat . List.intersperse [""] . filter (not . null)

showImport :: Types.ImportLine -> String
showImport (Types.ImportLine imp cmts _) =
    above ++ importLine ++ (if null right then "" else ' ' : right)
    where
    above = concat [cmt ++ "\n" | Types.Comment Types.CmtAbove cmt <- cmts]
    importLine = Haskell.prettyPrint imp
    right = Util.join "\n" [cmt | Types.Comment Types.CmtRight cmt <- cmts]
