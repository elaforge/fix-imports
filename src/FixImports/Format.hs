{-# LANGUAGE OverloadedStrings #-}
module FixImports.Format (
    formatGroups
    , showImport
) where
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Text.PrettyPrint as PP
import           Text.PrettyPrint ((<+>))

import qualified FixImports.Config as Config
import qualified FixImports.Types as Types
import qualified FixImports.Util as Util


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
formatGroups :: Config.Format -> Config.Order -> [Types.ImportLine] -> String
formatGroups format order imports =
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
        ( localPriority (Config._importOrder order) (name import_)
        , name import_
        , qualifiedPrio import_
        )
    qualifiedPrio = not . Types._importQualified . Types.importDecl
    name = Types._importName . Types.importDecl
    (unqualified, local, package) = Util.partition2
        ((Config._sortUnqualifiedLast order &&) . isUnqualified
            . Types.importDecl)
        ((==Types.Local) . Types.importSource)
        imports
    group
        | Config._groupImports format = collapse . Util.groupOn topModule
        | otherwise = (:[])
    topModule = takeWhile (/='.') . Types.moduleName . name
    collapse [] = []
    collapse (x:xs)
        | length x <= 2 = case collapse xs of
            [] -> [x]
            y : ys -> (x ++ y) : ys
        | otherwise = x : collapse xs
    showGroups = List.intercalate [""] . map (map (showImportLine format))
    joinGroups = List.intercalate [""] . filter (not . null)
    prelude = Types.ModuleName "Prelude"

isUnqualified :: Types.Import -> Bool
isUnqualified imp = not (Types._importQualified imp)
    && Maybe.isNothing (Types._importEntities imp)

-- | Modules whose top level element is in 'importFirst' go first, ones in
-- 'importLast' go last, and the rest go in the middle.
--
-- Like 'searchPrio' but for order.
localPriority :: Config.Priority Config.ModulePattern -> Types.ModuleName
    -> (Int, Maybe Int)
localPriority prio import_ =
    case List.findIndex (`Config.matchModule` import_) firsts of
        Just k -> (-1, Just k)
        Nothing -> case List.findIndex (`Config.matchModule` import_) lasts of
            Nothing -> (0, Nothing)
            Just k -> (1, Just k)
    where
    firsts = Config.high prio
    lasts = Config.low prio

showImportLine :: Config.Format -> Types.ImportLine -> String
showImportLine format (Types.ImportLine imp cmts _) = concat
    [ above
    , showImport format imp
    , (if null right then "" else ' ' : right)
    ]
    where
    above = concat [cmt ++ "\n" | Types.Comment Types.CmtAbove cmt <- cmts]
    right = Util.join "\n" [cmt | Types.Comment Types.CmtRight cmt <- cmts]

showImport :: Config.Format -> Types.Import -> String
showImport format
        (Types.Import name pkgQualifier source safe qualified as hiding
            entities _span) =
    PP.renderStyle style $ PP.hang
        (PP.hsep
            [ "import"
            , if qualified || not (Config._leaveSpaceForQualified format)
                    || source || safe
                then mempty
                else PP.text (replicate (length ("qualified" :: String)) ' ')
            , if source then "{-# SOURCE #-}" else mempty
            , if safe then "safe" else mempty
            , if qualified then "qualified" else mempty
            , maybe mempty (\s -> PP.text (show s)) pkgQualifier
            , PP.text $ Types.moduleName name
            , maybe mempty
                (\(Types.Qualification qual) -> "as" <+> PP.text qual) as
            , if hiding then "hiding" else mempty
            ])
        (Config._wrapIndent format) (maybe mempty prettyEntities entities)
    where
    style = PP.style
        { PP.lineLength = Config._columns format
        , PP.ribbonsPerLine = 1
        }

prettyEntities :: [Either Types.Error Types.Entity] -> PP.Doc
prettyEntities = parenList . map pp
    where
    -- This means 'extractEntity' hit something it didn't think should be
    -- possible.
    pp (Left err) = "{{parse error: " <> PP.text err <> "}}"
    pp (Right (Types.Entity qualifier var list)) =
        (maybe mempty PP.text qualifier <+> PP.text (Types.showName var))
        <> maybe mempty PP.text list

parenList :: [PP.Doc] -> PP.Doc
parenList = PP.parens . PP.fsep . PP.punctuate PP.comma
