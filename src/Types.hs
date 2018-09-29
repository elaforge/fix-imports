{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where
import qualified Data.String as String
import qualified Language.Haskell.Exts as Haskell
import qualified System.FilePath as FilePath


data ImportLine = ImportLine {
    importDecl :: ImportDecl
    , importComments :: [Comment]
    , importSource :: !Source
    } deriving (Show)

-- | Where did this import come from?
data Source = Local | Package deriving (Eq, Show)

-- | A Comment is associated with a particular import line.
data Comment = Comment CmtPos String deriving (Show)
data CmtPos = CmtAbove | CmtRight deriving (Show)

-- | A parsed import line.
type ImportDecl = Haskell.ImportDecl Haskell.SrcSpanInfo
type Module = Haskell.Module Haskell.SrcSpanInfo

-- | A Qualification is a qualified name minus the actual name.  So it should
-- be the tail of a ModuleName.
newtype Qualification = Qualification String
    deriving (Eq, Ord, Show, String.IsString)

-- | An unqualified identifier.
type Name = Haskell.Name Haskell.SrcSpanInfo

newtype ModuleName = ModuleName String
    deriving (Eq, Ord, Show, String.IsString)

moduleName :: ModuleName -> String
moduleName (ModuleName n) = n

pathToModule :: FilePath -> ModuleName
pathToModule = ModuleName
    .  map (\c -> if c == '/' then '.' else c) . FilePath.dropExtension

moduleToPath :: ModuleName -> FilePath
moduleToPath (ModuleName name) =
    map (\c -> if c == '.' then '/' else c) name ++ ".hs"

-- | Get the qualified name from an import.
importDeclQualification :: ImportDecl -> Qualification
importDeclQualification decl = moduleToQualification $
    maybe (Haskell.importModule decl) id (Haskell.importAs decl)

-- | Extract the ModuleName from an ImportDecl.
importDeclModule :: ImportDecl -> ModuleName
importDeclModule imp = case Haskell.importModule imp of
    Haskell.ModuleName _ s -> ModuleName s

importModule :: ImportLine -> ModuleName
importModule = importDeclModule . importDecl

-- | The parser represents the \'as\' part of an import as a ModuleName even
-- though it's actually a Qualification.
moduleToQualification :: Haskell.ModuleName Haskell.SrcSpanInfo
    -> Qualification
moduleToQualification (Haskell.ModuleName _ s) = Qualification s
