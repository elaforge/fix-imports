{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- {-# LANGUAGE NoMonomorphismRestriction #-}
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
import qualified Data.Maybe as Maybe
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set

import qualified Language.Haskell.GhclibParserEx.GHC.Parser as Parser
-- This is the new one, but I use the old versions of ghc-lib-parser-ex.
-- import qualified Language.Haskell.GhclibParserEx.GHC.Settings.Config as Config
import qualified Language.Haskell.GhclibParserEx.Config as Config
import qualified Language.Haskell.GhclibParserEx.GHC.Driver.Session as Session

import qualified ApiAnnotation
import qualified Bag
import qualified BasicTypes
import qualified FastString
import qualified DynFlags
import qualified ErrUtils
import qualified GHC.Hs as Hs
import qualified Lexer
import qualified Module
import qualified OccName
import qualified Outputable
import qualified RdrName
import qualified SrcLoc

import qualified Data.Generics.Uniplate.Data as Uniplate

import qualified FixImports.Types as Types
import qualified FixImports.Util as Util


-- * parse

type Module = Hs.HsModule Hs.GhcPs

-- | makeDynFlags forces parse into IO.  The reason seems to be that GHC
-- puts dyn flags in a global variable.
parse :: [Types.Extension] -> FilePath -> String
    -> IO (Either String (Module, [Comment]))
parse extensions filename src = makeDynFlags extensions filename src >>= \case
    Left err -> return $ Left $ "parsing pragmas: " <> err
    Right dynFlags -> return $
        extractParseResult dynFlags $ Parser.parseFile filename dynFlags src

makeDynFlags :: [Types.Extension] -> FilePath -> String
    -> IO (Either Types.Error DynFlags.DynFlags)
makeDynFlags extensions fname src = do
    Session.parsePragmasIntoDynFlags defaultDynFlags
        (extensions ++ defaultExtensions, []) fname src
    where
    defaultExtensions = DynFlags.languageExtensions Nothing

defaultDynFlags :: DynFlags.DynFlags
defaultDynFlags =
    DynFlags.defaultDynFlags Config.fakeSettings Config.fakeLlvmConfig

data Comment = Comment { _span :: !Types.SrcSpan, _comment :: !String }
    deriving (Eq, Ord, Show)

instance DeepSeq.NFData Comment where rnf (Comment _ cmt) = DeepSeq.rnf cmt

extractComment :: SrcLoc.Located ApiAnnotation.AnnotationComment -> Comment
extractComment cmt = Comment (extractSrcSpan (SrcLoc.getLoc cmt)) $
    case SrcLoc.unLoc cmt of
        ApiAnnotation.AnnDocCommentNext s -> s
        ApiAnnotation.AnnDocCommentPrev s -> s
        ApiAnnotation.AnnDocCommentNamed s -> s
        ApiAnnotation.AnnDocSection _depth s -> s
        ApiAnnotation.AnnDocOptions s -> s
        ApiAnnotation.AnnLineComment s -> s
        ApiAnnotation.AnnBlockComment s -> s

extractParseResult :: SrcLoc.HasSrcSpan a => DynFlags.DynFlags
    -> Lexer.ParseResult a -> Either String (SrcLoc.SrcSpanLess a, [Comment])
extractParseResult dynFlags = \case
    Lexer.POk state val -> Right
        ( SrcLoc.unLoc val
        , map extractComment (Lexer.comment_q state)
        -- Lexer.annotations_comments I think is supposed to have comments
        -- associated with their "attached" SrcSpan, whatever that is.
        -- In any case, it's empty for comments in the import block at least.
        )
    Lexer.PFailed state -> Left $ unlines $ concat
        [ map (("warn: "<>) . show) $ Bag.bagToList warns
        , map (renderSDoc dynFlags) $ ErrUtils.pprErrMsgBagWithLoc errors
        ]
        where (warns, errors) = Lexer.getMessages state dynFlags

renderSDoc :: DynFlags.DynFlags -> Outputable.SDoc -> String
renderSDoc dynFlags sdoc =
    Outputable.renderWithStyle dynFlags sdoc
        (Outputable.defaultErrStyle dynFlags)

-- * extract

-- | Qualifications of all the qualified names in this module.
qualifications :: Module -> Set.Set Types.Qualification
qualifications mod = Set.fromList
    [ Types.Qualification $ Module.moduleNameString qmod
    | RdrName.Qual qmod _occName <- Uniplate.universeBi mod
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
    RdrName.Unqual occName <- Uniplate.universeBi hsType
    let var = OccName.occNameString occName
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
    RdrName.Unqual occName <- Uniplate.universeBi rhss
    return $ inferName $ OccName.occNameString occName

-- | Return half-open line range of import block, starting from (0 based) line
-- of first import to the line after the last one.
importRange :: Module -> (Int, Int)
importRange mod =
    get . unzip . map range . Maybe.mapMaybe (getSpan . SrcLoc.getLoc)
        . Hs.hsmodImports $ mod
    where
    -- This range is 1-based inclusive, and I want 0-based half-open, so
    -- subtract 1 from the start.
    get (starts@(_:_), ends@(_:_)) = (minimum starts - 1, maximum ends)
    -- No imports, pick the line after export list or module header.
    get _ = fromMaybe (0, 0) $ do
        span <- getSpan =<<
            (SrcLoc.getLoc <$> Hs.hsmodExports mod)
            <|> (SrcLoc.getLoc <$> Hs.hsmodName mod)
        return (SrcLoc.srcSpanEndLine span, SrcLoc.srcSpanEndLine span)
    range span = (SrcLoc.srcSpanStartLine span, SrcLoc.srcSpanEndLine span)
    getSpan (SrcLoc.RealSrcSpan span) = Just span
    getSpan _ = Nothing

-- ** Import

extractImports :: Module -> [Types.Import]
extractImports = map extractImport . Hs.hsmodImports

extractImport :: SrcLoc.Located (Hs.ImportDecl Hs.GhcPs) -> Types.Import
extractImport locDecl = Types.Import
    { _importName = Types.ModuleName $ Module.moduleNameString $ SrcLoc.unLoc $
        Hs.ideclName decl
    , _importPkgQualifier = FastString.unpackFS . BasicTypes.sl_fs <$>
        Hs.ideclPkgQual decl
    , _importSource = Hs.ideclSource decl
    , _importSafe = Hs.ideclSafe decl
    -- I don't distinguish Hs.QualifiedPost
    , _importQualified = Hs.ideclQualified decl /= Hs.NotQualified
    , _importAs = Types.Qualification . Module.moduleNameString . SrcLoc.unLoc
        <$> Hs.ideclAs decl
    , _importHiding = maybe False fst $ Hs.ideclHiding decl
    , _importEntities = map (extractEntity . SrcLoc.unLoc) . SrcLoc.unLoc
        . snd <$> Hs.ideclHiding decl
    , _importSpan = extractSrcSpan (SrcLoc.getLoc locDecl)
    }
    where decl = SrcLoc.unLoc locDecl

extractSrcSpan :: SrcLoc.SrcSpan -> Types.SrcSpan
extractSrcSpan (SrcLoc.RealSrcSpan span) = Types.SrcSpan
    -- GHC SrcSpan has 1-based lines, I use 0-based ones.
    { _startLine = SrcLoc.srcSpanStartLine span - 1
    , _startCol = SrcLoc.srcSpanStartCol span
    , _endLine = SrcLoc.srcSpanEndLine span - 1
    , _endCol = SrcLoc.srcSpanEndCol span
    }
extractSrcSpan (SrcLoc.UnhelpfulSpan fstr) =
    error $ "UnhelpfulSpan: " <> show fstr
    -- I think GHC uses these internally, in phases after Hs.GhcPs.

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
    -- I think _labels is for records, but I don't think it's populated at the
    -- GhcPs stage, since how would it know?
    Hs.IEThingWith _ var _wildcard things _labels -> Right
        ( unvar var
        , Just $ "(" <> List.intercalate ", " (map (varStr . unvar) things)
            <> ")"
        )
    -- Shouldn't happen, export only.
    Hs.IEModuleContents {} -> Left "IEModuleContents"
    Hs.IEGroup {} -> Left "IEGroup"
    Hs.IEDoc {} -> Left "IEDoc"
    Hs.IEDocNamed {} -> Left "IEDocNamed"
    Hs.XIE {} -> Left "XIE"
    where
    entity ((qual, var), list) = Types.Entity qual var list
    unvar var = case SrcLoc.unLoc var of
        Hs.IEName n -> (Nothing, toName n)
        Hs.IEPattern n -> (Just "pattern", toName n)
        Hs.IEType n -> (Just "type", toName n)
    varStr (Just qual, name) = qual <> " " <> Types.showName name
    varStr (Nothing, name) = Types.showName name
    toName = inferName . unRdrName . SrcLoc.unLoc

inferName :: String -> Types.Name
inferName var
    | all Util.haskellOpChar var = Types.Operator var
    | otherwise = Types.Name var

unRdrName :: RdrName.RdrName -> String
unRdrName = \case
    RdrName.Unqual occName -> OccName.occNameString occName
    RdrName.Qual mod occName ->
        Module.moduleNameString mod <> "." <> OccName.occNameString occName
    -- TODO what is this?
    RdrName.Orig _mod occName -> -- modName mod <>
        "+" <> OccName.occNameString occName
    -- TODO what is this?
    RdrName.Exact _name -> "exact"
