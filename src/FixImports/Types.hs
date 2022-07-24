{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
module FixImports.Types (
    module FixImports.Types
    , module GHC.LanguageExtensions.Type
) where
import qualified Control.DeepSeq as DeepSeq
import           Control.DeepSeq (deepseq)
import qualified Data.Either as Either
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.String as String
import qualified System.FilePath as FilePath

import           "ghc-lib-parser" GHC.LanguageExtensions.Type (Extension(..))
import qualified "ghc-lib-parser-ex" Language.Haskell.GhclibParserEx.GHC.Driver.Session


type Error = String

parseExtension :: String -> Maybe Extension
parseExtension =
    Language.Haskell.GhclibParserEx.GHC.Driver.Session.readExtension

-- * ImportLine

data ImportLine = ImportLine {
    importDecl :: !Import
    , importComments :: ![Comment]
    , importSource :: !Source
    } deriving (Show)

instance DeepSeq.NFData ImportLine where
    rnf (ImportLine decl cmts source) =
        decl `seq` cmts `deepseq` source `deepseq` ()

-- | Where did this import come from?
data Source = Local | Package deriving (Eq, Show)

instance DeepSeq.NFData Source where
    rnf _ = ()

-- | A Comment is associated with a particular import line.
data Comment = Comment !CmtPos !String deriving (Show)
data CmtPos = CmtAbove | CmtRight deriving (Show)

instance DeepSeq.NFData Comment where
    rnf (Comment a b) = a `seq` b `seq` ()

-- * Import

data Import = Import {
    _importName :: !ModuleName
    , _importPkgQualifier :: !(Maybe String)
    -- | SOURCE pragma?  There is also ideclSourceSrc, but it looks like it's
    -- just to preserve the exact case or British-ness of the pragma, so I can
    -- discard it.
    , _importSource :: !Bool
    , _importSafe :: !Bool -- ^ safe import
    , _importQualified :: !Bool -- ^ qualified
    , _importAs :: !(Maybe Qualification) -- ^ import as
    , _importHiding :: !Bool -- ^ import list is hiding
    , _importEntities :: !(Maybe [Either Error Entity])
    , _importSpan :: !SrcSpan
    } deriving (Eq, Ord, Show)

instance DeepSeq.NFData Import where
    rnf imp = _importPkgQualifier `deepseq` _importEntities imp `deepseq` ()

data SrcSpan = SrcSpan { _startLine, _startCol, _endLine, _endCol :: !Int }
    deriving (Eq, Ord, Show)

noSpan :: SrcSpan
noSpan = SrcSpan 0 0 0 0

makeImport :: ModuleName -> Import
makeImport name = Import
    { _importName = name
    , _importPkgQualifier = Nothing
    , _importSource = False
    , _importSafe = False
    , _importQualified = False
    , _importAs = Nothing
    , _importHiding = False
    , _importEntities = Nothing
    , _importSpan = noSpan
    }

-- | Get the qualified name from an Import.
importQualification :: Import -> Qualification
importQualification imp =
    fromMaybe (toQual (_importName imp)) (_importAs imp)
    where toQual (ModuleName m) = Qualification m

setQualification :: Qualification -> Import -> Import
setQualification qual imp = imp
    { _importQualified = True
    , _importAs = if toQual (_importName imp) == qual then Nothing
        else Just qual
    } where toQual (ModuleName m) = Qualification m

-- | @import X hiding (x)@ doesn't count as an unqualified import, at least not
-- the kind that I manage.
importUnqualified :: Import -> Bool
importUnqualified imp =
    not (_importQualified imp) && not (_importHiding imp)

-- | Has an import list, but is empty.  This import is a no-op, except for
-- instances.
importEmpty :: Import -> Bool
importEmpty imp = case _importEntities imp of
    Just [] -> True
    _ -> False

-- | If this import has a import list, modify its contents.
importModify :: ([Entity] -> [Entity]) -> Import -> Import
importModify modify imp = case (_importHiding imp, _importEntities imp) of
    (False, Just errEntities) -> imp
        { _importEntities = Just $ map Left errs
            ++ map Right (normalize (modify entities))
        }
        where (errs, entities) = Either.partitionEithers errEntities
    _ -> imp
    where
    -- Keep entities unique and sorted.
    normalize = Set.toList . Set.fromList

-- | An imported entity, e.g. @import X (entity)@.
data Entity = Entity {
    _entityQualifier :: !(Maybe String)
    , _entityVar :: !Name
    , _entityList :: !(Maybe String)
    } deriving (Eq, Ord, Show)

instance DeepSeq.NFData Entity where
    rnf (Entity a b c) = a `deepseq` b `deepseq` c `deepseq` ()

-- | A Qualification is a qualified name minus the actual name.  So it should
-- be the tail of a ModuleName.
newtype Qualification = Qualification String
    deriving (Eq, Ord, Show, DeepSeq.NFData, String.IsString)

-- | An unqualified identifier.
data Name = Name !String | Operator !String
    deriving (Eq, Ord, Show)

instance DeepSeq.NFData Name where
    rnf (Name s) = DeepSeq.rnf s
    rnf (Operator s) = DeepSeq.rnf s

showName :: Name -> String
showName (Name s) = s
showName (Operator s) = "(" <> s <> ")"

newtype ModuleName = ModuleName String
    deriving (Eq, Ord, Show, DeepSeq.NFData, String.IsString)

moduleName :: ModuleName -> String
moduleName (ModuleName n) = n

pathToModule :: FilePath -> ModuleName
pathToModule = ModuleName
    .  map (\c -> if c == '/' then '.' else c) . FilePath.dropExtension

moduleToPath :: ModuleName -> FilePath
moduleToPath (ModuleName name) =
    map (\c -> if c == '.' then '/' else c) name ++ ".hs"
