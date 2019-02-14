-- | Per-project 'Config' and functions to interpret it.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-} -- sort keys only care about Ord
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Config where
import Control.Monad (foldM, unless)
import Data.Bifunctor (second)
import qualified Data.Char as Char
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.IO as Text.IO
import qualified Data.Tuple as Tuple

import qualified Language.Haskell.Exts as Haskell
import qualified Language.Haskell.Exts.Extension as Extension
import qualified System.FilePath as FilePath
import qualified System.IO as IO
import qualified Text.PrettyPrint as PP
import Text.PrettyPrint ((<+>))

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
    -- | Map unqualified names to the module to import for them.
    , _unqualified :: Map.Map (Haskell.Name ()) Types.ModuleName
    -- import-as: Data.Text.Lazy as DTL -> Map DTL Data.Text.Lazy
    , _qualifyAs :: Map.Map Types.Qualification Types.Qualification
    , _format :: Format
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

instance Semigroup Priorities where
    Priorities a1 b1 <> Priorities a2 b2 = Priorities (a1<>a2) (b1<>b2)
instance Monoid Priorities where
    mempty = Priorities mempty mempty

data Priority a = Priority { high :: [a], low :: [a] }
    deriving (Eq, Show)

instance Semigroup (Priority a) where
    Priority a1 b1 <> Priority a2 b2 = Priority (a1<>a2) (b1<>b2)
instance Monoid (Priority a) where
    mempty = Priority mempty mempty

data Format = Format {
    -- | How to format import lines.  Nothing to use the standard
    -- haskell-src-exts style with defaultMode.
    _ppConfig :: Maybe PPConfig
    -- | If true, group imports by their first component.
    , _groupImports :: Bool
    } deriving (Eq, Show)

data PPConfig = PPConfig {
    _leaveSpaceForQualified :: Bool
    } deriving (Eq, Show)

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
    , _modulePriority = mempty
    , _unqualified = mempty
    , _qualifyAs = mempty
    , _format = defaultFormat
    , _debug = False
    }

defaultFormat :: Format
defaultFormat = Format
    { _ppConfig = Nothing
    , _groupImports = True
    }

-- | Parse .fix-imports file.
parse :: Text -> (Config, [Text])
parse text = (config, errors)
    where
    commas = Text.intercalate ", "
    errors = map (".fix-imports: "<>) $ concat
        [ [ "duplicate fields: " <> commas duplicates | not (null duplicates) ]
        , [ "unrecognized fields: " <> commas unknownFields
          | not (null unknownFields)
          ]
        , [ "unknown language extensions: " <> commas unknownLanguage
          | not (null unknownLanguage)
          ]
        , [ "unqualified: " <> commas unknownUnqualified
          | not (null unknownUnqualified)
          ]
        , [ "qualify-as: " <> commas unknownQualifyAs
          | not (null unknownQualifyAs)
          ]
        , maybe [] ((:[]) . ("format: "<>)) formatError
        ]
    config = empty
        { _includes = getStrings "include"
        , _language = language
        , _order = Order
            { _importOrder = Priority
                { high = getStrings "import-order-first"
                , low = getStrings "import-order-last"
                }
            , _sortUnqualifiedLast = getBool "sort-unqualified-last"
            }
        , _modulePriority = Priorities
            { prioPackage = Priority
                { high = getStrings "prio-package-high"
                , low = getStrings "prio-package-low"
                }
            , prioModule = Priority
                { high = getModules "prio-module-high"
                , low = getModules "prio-module-low"
                }
            }
        , _unqualified = Map.fromList $ map Tuple.swap unqualified
        , _format = format
        , _qualifyAs = qualifyAs
        }
    (unknownUnqualified, unqualified) = Either.partitionEithers $
        parseUnqualified (Text.unwords (get "unqualified"))
    (unknownLanguage, language) = parseLanguage (get "language")
    (unknownQualifyAs, qualifyAs) =
        parseQualifyAs $ Text.unwords $ get "qualify-as"
    (format, formatError) = case parseFormat (get "format") of
        Right format -> (format, Nothing)
        Left err -> (defaultFormat, Just err)
    unknownFields = Map.keys fields List.\\ valid
    valid =
        [ "format"
        , "import-order-first", "import-order-last"
        , "include"
        , "language"
        , "prio-module-high", "prio-module-low"
        , "prio-package-high", "prio-package-low"
        , "qualify-as"
        , "sort-unqualified-last"
        , "unqualified"
        ]
    fields = Map.fromList sections
    sections = Index.parseSections text
    duplicates = map head $ filter ((>1) . length) $ List.group $ List.sort $
        map fst sections

    getModules = map (Types.ModuleName . Text.unpack) . get
    get k = Map.findWithDefault [] k fields
    getStrings = map Text.unpack . get
    getBool k = k `Map.member` fields

parseFormat :: [Text] -> Either Text Format
parseFormat = foldM set defaultFormat
    where
    set fmt "leave-space-for-qualified" = Right $ fmt
        { _ppConfig = Just $ PPConfig { _leaveSpaceForQualified = True } }
    set fmt "no-group" = Right $ fmt { _groupImports = False }
    set _ w = Left $ "unrecognized word: " <> showt w

-- |
-- "A.B(c); Q(r)" -> [Right ("A.B", "c"), Right ("Q", "r")]
parseUnqualified :: Text
    -> [Either Text (Types.ModuleName, Haskell.Name ())]
parseUnqualified = concatMap (parse . Text.break (=='('))  . Text.splitOn ";"
    where
    parse (pre, post)
        | Text.null pre && Text.null post = []
        | Text.null pre = [Left $ "no module name before " <> showt post]
        | Text.null post = [Left $ "no import after " <> showt pre]
        | Just imports <- hasParens post =
            map (parseUnqualifiedImport (Text.strip pre))
                (map Text.strip (Text.splitOn "," imports))
        | otherwise = [Left $ "expected parens: " <> showt post]

-- |
-- "A.B" "(c)" -> (Haskell.Ident () "c", "A.B")
-- "A.B" "((+))" -> (Haskell.Symbol () "+", "A.B")
parseUnqualifiedImport :: Text -> Text
    -> Either Text (Types.ModuleName, Haskell.Name ())
parseUnqualifiedImport pre post = do
    unless (Text.all isModuleChar pre) $
        Left $ "this doesn't look like a module name: " <> showt pre
    let module_ = Types.ModuleName (Text.unpack pre)
    case hasParens post of
        Just op
            | Text.all Util.haskellOpChar op ->
                Right (module_, Haskell.Symbol () (Text.unpack op))
            | otherwise -> Left $ "non-symbols in operator: " <> showt post
        Nothing
            | Text.all (not . Util.haskellOpChar) post ->
                Right (module_, Haskell.Ident () (Text.unpack post))
            | otherwise ->
                Left $ "symbol char in id, use parens: " <> showt post
    where
    isModuleChar c = Char.isLetter c || Char.isDigit c || c == '.'

-- |
-- "A.B as AB; C as E" -> [("AB", "A.B"), ("E", "C")]
parseQualifyAs :: Text
    -> ([Text], Map.Map Types.Qualification Types.Qualification)
parseQualifyAs field
    | Text.null field = ([], mempty)
    | otherwise = second Map.fromList . Either.partitionEithers
        . map (parse . Text.words) . Text.splitOn ";" $ field
    where
    parse [module_, "as", alias] = Right
        ( Types.Qualification (Text.unpack alias)
        , Types.Qualification (Text.unpack module_)
        )
    parse ws = Left $ "stanza should look like 'ModuleName as X':"
        <> Text.unwords ws

hasParens :: Text -> Maybe Text
hasParens s
    | "(" `Text.isPrefixOf` s && ")" `Text.isSuffixOf` s =
        Just $ Text.drop 1 $ Text.dropEnd 1 s
    | otherwise = Nothing

parseLanguage :: [Text] -> ([Text], [Extension.Extension])
parseLanguage = Either.partitionEithers . map parse
    where
    parse w = case Extension.parseExtension (Text.unpack w) of
        Extension.UnknownExtension _ -> Left w
        ext -> Right ext

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
formatGroups :: Format -> Order -> [Types.ImportLine] -> String
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
    group
        | _groupImports format = collapse . Util.groupOn topModule
        | otherwise = (:[])
    topModule = takeWhile (/='.') . Types.moduleName . Types.importModule
    collapse [] = []
    collapse (x:xs)
        | length x <= 2 = case collapse xs of
            [] -> [x]
            y : ys -> (x ++ y) : ys
        | otherwise = x : collapse xs
    showGroups = List.intercalate [""] . map (map (showImport format))
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

showImport :: Format -> Types.ImportLine -> String
showImport format (Types.ImportLine decl cmts _) =
    above ++ showImportDecl format decl
        ++ (if null right then "" else ' ' : right)
    where
    above = concat [cmt ++ "\n" | Types.Comment Types.CmtAbove cmt <- cmts]
    right = Util.join "\n" [cmt | Types.Comment Types.CmtRight cmt <- cmts]

showImportDecl :: Format -> Types.ImportDecl -> String
showImportDecl format = case _ppConfig format of
    Nothing -> Haskell.prettyPrintStyleMode style Haskell.defaultMode
    Just config -> PP.renderStyle style . prettyImportDecl config
    where
    style = Haskell.style
        { Haskell.lineLength = 80
        , Haskell.ribbonsPerLine = 1
        }

prettyImportDecl :: PPConfig -> Haskell.ImportDecl a -> PP.Doc
prettyImportDecl config
        (Haskell.ImportDecl _ m qual src safe mbPkg mbName mbSpecs) = do
    PP.hsep
        [ "import"
        , if qual || not (_leaveSpaceForQualified config) then mempty
            else PP.text (replicate (length ("qualified" :: String)) ' ')
        , if src then "{-# SOURCE #-}" else mempty
        , if safe then "safe" else mempty
        , if qual then "qualified" else mempty
        , maybe mempty (\s -> PP.text (show s)) mbPkg
        , nameOf m
        , maybe mempty (\m -> "as" <+> nameOf m) mbName
        , maybe mempty prettyImportSpecList mbSpecs
        ]
        where
        nameOf (Haskell.ModuleName _ s) = PP.text s

-- ** Language.Haskell.Exts.Pretty copy-paste

-- Language.Haskell.Exts.Pretty doesn't export the pretty method.
--
-- Ironically the haskell-src-exts complains about having to copy paste
-- PP.punctuate, which is no longer necessary, while forcing me to copy paste
-- much more, by committing the same mistake.

prettyImportSpecList :: Haskell.ImportSpecList a -> PP.Doc
prettyImportSpecList (Haskell.ImportSpecList _ b ispecs) =
    (if b then "hiding" else mempty) <+> parenList (map prettyISpec ispecs)

prettyISpec :: Haskell.ImportSpec a -> PP.Doc
prettyISpec = \case
    Haskell.IVar _ name -> prettyName name
    Haskell.IAbs _ ns name -> prettyNamespace ns <+> prettyName name
    Haskell.IThingAll _ name -> prettyName name <> PP.text "(..)"
    Haskell.IThingWith _ name nameList ->
        prettyName name <> (parenList . map prettyCName $ nameList)

prettyName :: Haskell.Name l -> PP.Doc
prettyName name = case name of
    Haskell.Symbol _ ('#':_) -> PP.char '(' <+> ppName name <+> PP.char ')'
    _ -> parensIf (isSymbolName name) (ppName name)

prettyCName :: Haskell.CName l -> PP.Doc
prettyCName = \case
    Haskell.VarName _ n -> prettyName n
    Haskell.ConName _ n -> prettyName n

prettyNamespace :: Haskell.Namespace l -> PP.Doc
prettyNamespace = \case
    Haskell.NoNamespace {} -> mempty
    Haskell.TypeNamespace {} -> "type"
    Haskell.PatternNamespace {} -> "pattern"

parensIf :: Bool -> PP.Doc -> PP.Doc
parensIf True = PP.parens
parensIf False = id

isSymbolName :: Haskell.Name l -> Bool
isSymbolName (Haskell.Symbol {}) = True
isSymbolName _ = False


ppName :: Haskell.Name l -> PP.Doc
ppName (Haskell.Ident _ s) = PP.text s
ppName (Haskell.Symbol _ s) = PP.text s

parenList :: [PP.Doc] -> PP.Doc
parenList = PP.parens . PP.hsep . PP.punctuate PP.comma

pretty :: Haskell.Pretty a => a -> PP.Doc
pretty _ = "?"


-- * log

debug :: Config -> Text -> IO ()
debug config msg
    | _debug config = Text.IO.hPutStrLn IO.stderr msg
    | otherwise = return ()

showt :: Show a => a -> Text
showt = Text.pack . show
