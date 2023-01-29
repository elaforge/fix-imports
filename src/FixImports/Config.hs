{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-} -- sort keys only care about Ord
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
-- | Parse the config file.
module FixImports.Config where
import           Control.Monad (foldM, unless)
import           Data.Bifunctor (second)
import qualified Data.Char as Char
import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Data.Text.IO as Text.IO
import qualified Data.Tuple as Tuple

import qualified System.FilePath as FilePath
import qualified System.IO as IO
import qualified Text.Read as Read

import qualified FixImports.Index as Index
import qualified FixImports.Types as Types
import qualified FixImports.Util as Util


data Config = Config {
    -- | Additional directories to search for local modules.  Taken from the
    -- -i flag and 'include' config line.
    _includes :: [FilePath]
    -- | These language extensions are enabled by default.
    , _language :: [Types.Extension]
    -- | Import sort order.  Used by 'formatGroups'.
    , _order :: Order
    -- | Heuristics to pick the right module.  Used by 'pickModule'.
    , _modulePriority :: Priorities
    -- | Map unqualified names to the module to import for them.
    , _unqualified :: Map.Map Types.Name Types.ModuleName
    -- | Map abbreviation to the complete qualification:
    -- > import-as: Data.Text.Lazy as DTL -> [("DTL", "Data.Text.Lazy")]
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
    -- | If true, group imports by their first component.
    _groupImports :: Bool
    -- | Insert space for unqualified imports to make the modules line up.
    , _leaveSpaceForQualified :: Bool
    -- | Number of columns to wrap to.
    , _columns :: Int
    -- | How many spaces to indent a wrapped line.
    , _wrapIndent :: Int
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
    { _groupImports = True
    , _leaveSpaceForQualified = False
    , _columns = 80
    , _wrapIndent = 4
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
    fields = fmap (concatMap Text.words) $ Map.fromList sections
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
    set fmt "leave-space-for-qualified" = Right $
        fmt { _leaveSpaceForQualified = True }
    set fmt "no-group" = Right $ fmt { _groupImports = False }
    set fmt w | Just cols <- Text.stripPrefix "columns=" w =
        case Read.readMaybe (Text.unpack cols) of
            Nothing -> Left $ "non-numeric: " <> w
            Just cols -> Right $ fmt { _columns = cols }
    set _ w = Left $ "unrecognized word: " <> showt w

-- |
-- "A.B(c); Q(r)" -> [Right ("A.B", "c"), Right ("Q", "r")]
parseUnqualified :: Text -> [Either Text (Types.ModuleName, Types.Name)]
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
-- "A.B" "(c)" -> (Types.Name "c", "A.B")
-- "A.B" "((+))" -> (Types.Operator "+", "A.B")
parseUnqualifiedImport :: Text -> Text
    -> Either Text (Types.ModuleName, Types.Name)
parseUnqualifiedImport pre post = do
    unless (Text.all isModuleChar pre) $
        Left $ "this doesn't look like a module name: " <> showt pre
    let module_ = Types.ModuleName (Text.unpack pre)
    case hasParens post of
        Just op
            | Text.all Util.haskellOpChar op ->
                Right (module_, Types.Operator (Text.unpack op))
            | otherwise -> Left $ "non-symbols in operator: " <> showt post
        Nothing
            | Text.all (not . Util.haskellOpChar) post ->
                Right (module_, Types.Name (Text.unpack post))
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

parseLanguage :: [Text] -> ([Text], [Types.Extension])
parseLanguage = Either.partitionEithers . map parse
    where parse w = maybe (Left w) Right $ Types.parseExtension $ Text.unpack w

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


-- * log

debug :: Config -> Text -> IO ()
debug config msg
    | _debug config = Text.IO.hPutStrLn IO.stderr msg
    | otherwise = return ()

showt :: Show a => a -> Text
showt = Text.pack . show
