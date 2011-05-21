module Types where
import qualified Language.Haskell.Exts.Annotated as Haskell


data ImportLine = ImportLine {
    importDecl :: ImportDecl
    , importComments :: [Comment]
    , importIsLocal :: Bool
    } deriving (Show)

-- | A Comment is associated with a particular import line.
data Comment = Comment CmtPos String deriving (Show)
data CmtPos = CmtAbove | CmtRight deriving (Show)

type ImportDecl = Haskell.ImportDecl Haskell.SrcSpanInfo
type Module = Haskell.Module Haskell.SrcSpanInfo

-- | A Qualification is a qualified name minus the actual name.  So it should
-- be the tail of a ModuleName.
newtype Qualification = Qualification String
    deriving (Eq, Ord, Show)

newtype ModuleName = ModuleName String
    deriving (Eq, Ord, Show)

moduleName :: ModuleName -> String
moduleName (ModuleName n) = n

-- | Get the qualified name from a qualified import, if it is a qualified
-- import.
importDeclQualification :: ImportDecl -> Maybe Qualification
importDeclQualification decl
    | Haskell.importQualified decl = Just $ moduleToQualification $
        maybe (Haskell.importModule decl) id (Haskell.importAs decl)
    | otherwise = Nothing

-- | Extract the ModuleName from an ImportDecl.
importDeclModule :: ImportDecl -> ModuleName
importDeclModule imp = case Haskell.importModule imp of
    Haskell.ModuleName _ s -> ModuleName s

importModule :: ImportLine -> ModuleName
importModule = importDeclModule . importDecl

-- | The parser represents the \'as\' part of an import as a ModuleName even
-- though it's actually a Qualification.
moduleToQualification :: Haskell.ModuleName Haskell.SrcSpanInfo
    -> Types.Qualification
moduleToQualification (Haskell.ModuleName _ s) = Types.Qualification s
