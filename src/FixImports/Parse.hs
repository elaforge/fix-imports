{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-} -- without it extractEntity unvar unhappy
module FixImports.Parse (
    -- * types
    Module
    , Comment(..)
    -- * parse
    , parse
    -- * extract
    , qualifications
    , unqualifieds
    , importRange
    -- ** Import
    , extractImports

    -- TESTING
    , unqualifiedValues, unqualifiedTypes
) where
import           Control.Applicative ((<|>))
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Monad as Monad
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe as Maybe
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set

import qualified Language.Haskell.GhclibParserEx.GHC.Parser as Parser
import qualified Language.Haskell.GhclibParserEx.GHC.Settings.Config as Config
import qualified Language.Haskell.GhclibParserEx.GHC.Driver.Session as ExSession

import qualified GHC.Data.FastString as FastString
import qualified GHC.Data.Strict as Strict
import qualified GHC.Driver.Session as Session
import qualified GHC.Driver.Ppr as Ppr
import qualified GHC.Hs as Hs
import qualified GHC.Hs.DocString as DocString
import qualified GHC.Parser.Annotation as Annotation
import qualified GHC.Parser.Lexer as Lexer
import qualified GHC.Types.Error as Error
import qualified GHC.Types.Name.Occurrence as Occurrence
import qualified GHC.Types.Name.Reader as Reader
import qualified GHC.Types.SourceText as SourceText
import qualified GHC.Types.SrcLoc as SrcLoc
import qualified GHC.Types.PkgQual as PkgQual
import qualified GHC.Data.Bag as Bag
import qualified GHC.Unit.Types as Unit.Types
import qualified GHC.Utils.Error as Utils.Error
import qualified Language.Haskell.Syntax.Module.Name as Name

import qualified Data.Generics.Uniplate.Data as Uniplate

import qualified FixImports.Types as Types
import qualified FixImports.Util as Util


-- * parse

type Module = Hs.HsModule Hs.GhcPs

type Src = String

-- | makeDynFlags forces parse into IO.  The reason seems to be that GHC
-- puts dyn flags in a global variable.
parse :: [Types.Extension] -> FilePath -> Src
    -> IO (Either String (Module, [Comment]))
parse extensions filename src = makeDynFlags extensions filename src >>= \case
    Left err -> return $ Left $ "parsing pragmas: " <> err
    Right dynFlags -> return $ parseFile filename dynFlags src

data Comment = Comment { _span :: !Types.SrcSpan, _comment :: !String }
    deriving (Eq, Ord, Show)

instance DeepSeq.NFData Comment where rnf (Comment _ cmt) = DeepSeq.rnf cmt

makeDynFlags :: [Types.Extension] -> FilePath -> Src
    -> IO (Either String Session.DynFlags)
makeDynFlags extensions fname src =
    ExSession.parsePragmasIntoDynFlags defaultDynFlags
        (extensions ++ defaultExtensions, []) fname src
    where defaultExtensions = Session.languageExtensions Nothing

defaultDynFlags :: Session.DynFlags
defaultDynFlags = Session.defaultDynFlags Config.fakeSettings

-- | Parser.parseFile seems to return an unlifted type, even though I can't
-- tell from the :t or the source.  But it means you can't pass its result
-- to a toplevel function.  Weird.
parseFile :: FilePath -> Session.DynFlags -> String
    -> Either String (Module, [Comment])
parseFile filename dynFlags src = case Parser.parseFile filename dynFlags src of
    Lexer.POk state val -> Right
        ( SrcLoc.unLoc val
        -- , []
        , List.sort $ map extractComment (Lexer.comment_q state)
            ++ case Lexer.header_comments state of
                Strict.Nothing -> []
                Strict.Just cs -> map extractComment cs
            ++ importComments (SrcLoc.unLoc val)
        -- Lexer.annotations_comments I think is supposed to have comments
        -- associated with their "attached" SrcSpan, whatever that is.
        -- In any case, it's empty for comments in the import block at least.
        )
    Lexer.PFailed state -> Left $ unlines $ concat
        [ map ("warn: "<>) (extract warns)
        -- Looks like errors already start with "error: ".  Dunno about warns.
        , extract errors
        ]
        where
        (warns, errors) = Lexer.getPsMessages state
        extract =
            map (Ppr.showSDoc dynFlags . Utils.Error.pprLocMsgEnvelopeDefault)
            . Bag.bagToList . Error.getMessages

-- | Simplify a comment, this means I lose all the docstring details, but
-- I think it's ok.
extractComment :: Hs.LEpaComment -> Comment
extractComment cmt =
    Comment (extractRealSrcSpan (Annotation.anchor (SrcLoc.getLoc cmt))) $
        case Annotation.ac_tok (SrcLoc.unLoc cmt) of
            Annotation.EpaDocComment doc -> case doc of
                DocString.MultiLineDocString _decorator cs ->
                    mconcat $ map (DocString.unpackHDSC . SrcLoc.unLoc) $
                        NonEmpty.toList cs
                DocString.NestedDocString _decorator c ->
                    DocString.unpackHDSC (SrcLoc.unLoc c)
                DocString.GeneratedDocString c -> DocString.unpackHDSC c
            Annotation.EpaLineComment s -> s
            Annotation.EpaBlockComment s -> s
            Annotation.EpaDocOptions s -> s
            Annotation.EpaEofComment -> ""

-- * extract

importComments :: Module -> [Comment]
importComments =
    map extractComment
        . concatMap (extract . extractAnn . Annotation.ann . SrcLoc.getLoc)
        . Hs.hsmodImports
    where
    extractAnn = \case
        Annotation.EpAnn _anchor _anns comments -> comments
        Annotation.EpAnnNotUsed -> Annotation.emptyComments
    extract = \case
        Annotation.EpaComments prior -> prior
        Annotation.EpaCommentsBalanced prior following -> prior ++ following

-- | Qualifications of all the qualified names in this module.
qualifications :: Module -> Set.Set Types.Qualification
qualifications mod = Set.fromList
    [ Types.Qualification $ Name.moduleNameString moduleName
    | Reader.Qual moduleName _occName <- Uniplate.universeBi mod
    ]

-- | All unqualified names which are referenced in the module.  This is a lot
-- more complicated than 'qualifications' because this needs to omit unqualified
-- names which are defined in here.
unqualifieds :: Module -> Set.Set Types.Name
unqualifieds mod = unqualifiedTypes mod <> unqualifiedValues mod

-- | TODO: this doesn't handle type operators yet.
unqualifiedTypes :: Module -> Set.Set Types.Name
unqualifiedTypes mod = Set.fromList $ do
    hsType :: Hs.HsType Hs.GhcPs <- Uniplate.universeBi mod
    Reader.Unqual occName <- Uniplate.universeBi hsType
    let var = Occurrence.occNameString occName
    -- I don't want type variables, since they aren't references.  I can just
    -- filter out lower-case, which seems to be good enough?
    Monad.guard $ case var of
        c : _ -> Char.isUpper c
        _ -> False
    return $ inferName var

unqualifiedValues :: Module -> Set.Set Types.Name
unqualifiedValues mod = Set.fromList $ do
    (Hs.FunBind { fun_matches } :: Hs.HsBindLR Hs.GhcPs Hs.GhcPs)
        <- Uniplate.universeBi mod
    let matches = map SrcLoc.unLoc $ SrcLoc.unLoc $ Hs.mg_alts fun_matches
    -- I think 'pats' is the binding names.
    -- let pats = concatMap Hs.m_pats matches
    let rhss = map Hs.m_grhss matches
    Reader.Unqual occName <- Uniplate.universeBi rhss
    return $ inferName $ Occurrence.occNameString occName

-- | Return half-open line range of import block, starting from (0 based) line
-- of first import to the line after the last one.
importRange :: Module -> (Int, Int)
importRange mod =
    get . unzip . map range
        . Maybe.mapMaybe (getSpan . Annotation.locA . SrcLoc.getLoc)
        . Hs.hsmodImports $ mod
    where
    -- This range is 1-based inclusive, and I want 0-based half-open, so
    -- subtract 1 from the start.
    get :: ([Int], [Int]) -> (Int, Int)
    get (starts@(_:_), ends@(_:_)) = (minimum starts - 1, maximum ends)
    -- No imports, pick the line after export list or module header.
    get _ = fromMaybe (0, 0) $ do
        span <- getSpan =<<
            (Annotation.locA . SrcLoc.getLoc <$> Hs.hsmodExports mod)
            <|> (Annotation.locA . SrcLoc.getLoc <$> Hs.hsmodName mod)
        return (SrcLoc.srcSpanEndLine span, SrcLoc.srcSpanEndLine span)
    range span = (SrcLoc.srcSpanStartLine span, SrcLoc.srcSpanEndLine span)
    getSpan (SrcLoc.RealSrcSpan span _) = Just span
    getSpan _ = Nothing

-- ** Import

extractImports :: Module -> [Types.Import]
extractImports = map extractImport . Hs.hsmodImports

extractImport :: Hs.LImportDecl Hs.GhcPs -> Types.Import
extractImport locDecl = Types.Import
    { _importName = Types.ModuleName $ Name.moduleNameString $ SrcLoc.unLoc $
        Hs.ideclName decl
    , _importPkgQualifier = case Hs.ideclPkgQual decl of
        PkgQual.NoRawPkgQual -> Nothing
        PkgQual.RawPkgQual stringLit ->
            Just $ FastString.unpackFS $ SourceText.sl_fs stringLit
    , _importIsBoot = Hs.ideclSource decl == Unit.Types.IsBoot
    , _importSafe = Hs.ideclSafe decl
    -- I don't distinguish Hs.QualifiedPost
    , _importQualified = Hs.ideclQualified decl /= Hs.NotQualified
    , _importAs = Types.Qualification . Name.moduleNameString . SrcLoc.unLoc
        <$> Hs.ideclAs decl
    , _importHiding = case Hs.ideclImportList decl of
        Just (Hs.EverythingBut, _) -> True
        _ -> False
    , _importEntities = case Hs.ideclImportList decl of
        Just (_, things) ->
            Just $ map (extractEntity . SrcLoc.unLoc) $ SrcLoc.unLoc things
        _ -> Nothing
    , _importSpan = extractSrcSpan $ Annotation.locA $ SrcLoc.getLoc locDecl

    }
    where decl = SrcLoc.unLoc locDecl

extractSrcSpan :: SrcLoc.SrcSpan -> Types.SrcSpan
extractSrcSpan (SrcLoc.RealSrcSpan span _) = extractRealSrcSpan span
extractSrcSpan (SrcLoc.UnhelpfulSpan fstr) =
    error $ "UnhelpfulSpan: " <> show fstr
    -- I think GHC uses these internally, in phases after Hs.GhcPs.

extractRealSrcSpan :: SrcLoc.RealSrcSpan -> Types.SrcSpan
extractRealSrcSpan span = Types.SrcSpan
    -- GHC SrcSpan has 1-based lines, I use 0-based ones.
    { _startLine = SrcLoc.srcSpanStartLine span - 1
    , _startCol = SrcLoc.srcSpanStartCol span
    , _endLine = SrcLoc.srcSpanEndLine span - 1
    , _endCol = SrcLoc.srcSpanEndCol span
    }

extractEntity :: Hs.IE Hs.GhcPs -> Either Types.Error Types.Entity
extractEntity = fmap entity . \case
    -- var
    Hs.IEVar _ var -> Right (unvar var, Nothing)
    -- Constructor
    Hs.IEThingAbs _ var -> Right (unvar var, Nothing)
    -- Constructor(..)
    Hs.IEThingAll _ var -> Right (unvar var, Just "(..)")
    -- Constructor(x, y)
    -- What is _wildcard?
    Hs.IEThingWith _ var _wildcard things -> Right
        ( unvar var
        , Just $ "(" <> List.intercalate ", " (map (varStr . unvar) things)
            <> ")"
        )
    -- Shouldn't happen, export only.
    Hs.IEModuleContents {} -> Left "IEModuleContents"
    Hs.IEGroup {} -> Left "IEGroup"
    Hs.IEDoc {} -> Left "IEDoc"
    Hs.IEDocNamed {} -> Left "IEDocNamed"
    where
    entity ((qual, var), list) = Types.Entity qual var list
    unvar var = case SrcLoc.unLoc var of
        Hs.IEName _ n -> (Nothing, toName n)
        Hs.IEPattern _ n -> (Just "pattern", toName n)
        Hs.IEType _ n -> (Just "type", toName n)
        -- It's a DataConCantHappen, so I think supposed to not happen?
        Hs.XIEWrappedName _ -> (Just "??", Types.Name "XIEWrappedName")
    varStr (Just qual, name) = qual <> " " <> Types.showName name
    varStr (Nothing, name) = Types.showName name
    toName = inferName . unRdrName . SrcLoc.unLoc

inferName :: String -> Types.Name
inferName var
    | all Util.haskellOpChar var = Types.Operator var
    | otherwise = Types.Name var

unRdrName :: Reader.RdrName -> String
unRdrName = \case
    Reader.Unqual occName -> Occurrence.occNameString occName
    Reader.Qual mod occName ->
        Name.moduleNameString mod <> "." <> Occurrence.occNameString occName
    -- TODO what is this?
    Reader.Orig _mod occName -> -- modName mod <>
        "+" <> Occurrence.occNameString occName
    -- TODO what is this?
    Reader.Exact _name -> "exact"
