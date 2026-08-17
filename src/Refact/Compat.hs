{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms #-}

module Refact.Compat (
  -- * ApiAnnotation / GHC.Parser.ApiAnnotation
#if MIN_VERSION_ghc(9,12,0)
#else
  AnnKeywordId (..),
#endif
  DeltaPos(..),

  -- * BasicTypes / GHC.Types.Basic
  Fixity(..),
  SourceText (..),

  -- * DynFlags / GHC.Driver.Session
  FlagSpec (..),
  GeneralFlag (..),
  OnOff (..),
  gopt_set,
  gopt_unset,
  parseDynamicFilePragma,
  xopt_set,
  xopt_unset,
  xFlags,

  -- * ErrUtils
  Errors,
  ErrorMessages,
  onError,

  -- * FastString / GHC.Data.FastString
  FastString,
  mkFastString,

  -- * HeaderInfo / GHC.Parser.Header
  getOptions,

  -- * HsExpr / GHC.Hs.Expr
  GRHS (..),
  HsExpr (..),
  HsMatchContext (..),
  HsStmtContext (..),
  Match (..),
  MatchGroup (..),
  StmtLR (..),

  -- * HsSyn / GHC.Hs
  module GHC.Hs,

  -- * Name / OccName / GHC.Types.Name
  nameOccName,
  occName,
  occNameString,
  ppr,

  -- * Outputable / GHC.Utils.Outputable
  showSDocUnsafe,

  -- * Panic / GHC.Utils.Panic
  handleGhcException,

  -- * RdrName / GHC.Types.Name.Reader
  RdrName (..),
  rdrNameOcc,

  -- * SrcLoc / GHC.Types.SrcLoc
  GenLocated (..),
  pattern RealSrcLoc',
  pattern RealSrcSpan',
  RealSrcSpan (..),
  SrcSpanLess,
  combineSrcSpans,
  composeSrcSpan,
  decomposeSrcSpan,

  -- * StringBuffer
  stringToStringBuffer,

  -- * Misc
  impliedXFlags,

  -- * Non-GHC stuff
  -- AnnKeyMap,
  FunBind,
  DoGenReplacement,
  Module,
  MonadFail',
  ReplaceWorker,
  annSpanToSrcSpan,
  badAnnSpan,
  mkErr,
  parseModuleName,
  setAnnSpanFile,
  setRealSrcSpanFile,
  setSrcSpanFile,
  srcSpanToAnnSpan,
  AnnSpan,
  commentSrcSpan,
  ann,
  transferEntryDP,
  transferEntryDP',
  AnnConstraint,
  showAst,

#if MIN_VERSION_ghc(9,4,0)
  -- * GHC 9.4 stuff
  initParserOpts,
#endif
) where

import Control.Monad.Trans.State.Strict (StateT)
#if MIN_VERSION_ghc(9,12,0)
import Data.Data (Data, Typeable)
#else
import Data.Data (Data)
#endif
import qualified GHC
import GHC.Data.Bag (unitBag, bagToList)
import GHC.Data.FastString (FastString, mkFastString)
#if MIN_VERSION_ghc(9,4,0)
import qualified GHC.Data.Strict as Strict
#endif
import GHC.Data.StringBuffer (StringBuffer, stringToStringBuffer)
#if MIN_VERSION_ghc(9,4,0)
import GHC.Driver.Config.Parser
import GHC.Driver.Errors.Types (ErrorMessages, ghcUnknownMessage, GhcMessage)
#endif
#if MIN_VERSION_ghc(9,14,0)
import GHC.Driver.Session hiding (initDynFlags, parseDynamicFilePragma)
import qualified GHC.Driver.Session as Session (parseDynamicFilePragma)
#else
import GHC.Driver.Session hiding (initDynFlags, impliedXFlags, parseDynamicFilePragma)
import qualified GHC.Driver.Session as Session (impliedXFlags, parseDynamicFilePragma)
import GHC.LanguageExtensions.Type (Extension)
#endif
#if MIN_VERSION_ghc(9,14,0)
import GHC.Hs hiding (Pat, Stmt, parseModuleName)
#elif MIN_VERSION_ghc(9,6,0)
import GHC.Hs hiding (Pat, Stmt, parseModuleName, ann)
#else
import GHC.Hs hiding (Pat, Stmt, ann)
#endif
import qualified GHC.Parser.Header as Header (getOptions)
#if MIN_VERSION_ghc(9,8,0)
import GHC.Types.Error (defaultDiagnosticOpts, getMessages)
#elif MIN_VERSION_ghc(9,4,0)
import GHC.Types.Error (getMessages)
#endif
import GHC.Types.Fixity  ( Fixity(..) )
import GHC.Types.Name (nameOccName, occName, occNameString)
import GHC.Types.Name.Reader (RdrName (..), rdrNameOcc)
import GHC.Types.SrcLoc hiding (spans)
import GHC.Types.SourceText
#if MIN_VERSION_ghc(9,4,0)
import GHC.Utils.Error
#else
import GHC.Utils.Error hiding (mkErr)
#endif
#if MIN_VERSION_ghc(9,14,0)
import GHC.Utils.Logger (initLogger)
#endif
import GHC.Utils.Outputable
  ( ppr,
    showSDocUnsafe,
    text,
    vcat,
  )
import GHC.Utils.Panic
  ( handleGhcException
  , pprPanic
  )
import Language.Haskell.GHC.ExactPrint.Parsers (Parser)
import Language.Haskell.GHC.ExactPrint.Utils
import Refact.Types (Refactoring)
#if MIN_VERSION_ghc(9,12,0)
import qualified Language.Haskell.GHC.ExactPrint.Transform as Exact
#else
import Language.Haskell.GHC.ExactPrint (transferEntryDP, transferEntryDP', showAst)
#endif


#if MIN_VERSION_ghc(9,12,0)
type AnnConstraint an = (NoAnn an, Semigroup an)
#else
type AnnConstraint an = (Monoid an)
#endif

type MonadFail' = MonadFail

#if MIN_VERSION_ghc(9,6,0)
type Module = Located (HsModule GhcPs)
#else
type Module = Located HsModule
#endif

type Errors = ErrorMessages

onError :: String -> Errors -> a
onError s = pprPanic s . vcat . ppp

ppp :: Errors -> [SDoc]
#if MIN_VERSION_ghc(9,6,0)
ppp = concatMap (unDecorated . diagnosticMessage (defaultDiagnosticOpts @GhcMessage) . errMsgDiagnostic) . bagToList . getMessages
#elif MIN_VERSION_ghc(9,4,0)
ppp = concatMap (unDecorated . diagnosticMessage . errMsgDiagnostic) . bagToList . getMessages
#else
ppp = concatMap (unDecorated . errMsgDiagnostic) . bagToList
#endif

#if MIN_VERSION_ghc(9,12,0)
type FunBind = HsMatchContext (LocatedN RdrName)
#else
type FunBind = HsMatchContext GhcPs
#endif

pattern RealSrcLoc' :: RealSrcLoc -> SrcLoc
pattern RealSrcLoc' r <- RealSrcLoc r _ where
#if MIN_VERSION_ghc(9,4,0)
  RealSrcLoc' r = RealSrcLoc r Strict.Nothing
#else
  RealSrcLoc' r = RealSrcLoc r Nothing
#endif
{-# COMPLETE RealSrcLoc', UnhelpfulLoc #-}

pattern RealSrcSpan' :: RealSrcSpan -> SrcSpan
pattern RealSrcSpan' r <- RealSrcSpan r _ where
#if MIN_VERSION_ghc(9,4,0)
  RealSrcSpan' r = RealSrcSpan r Strict.Nothing
#else
  RealSrcSpan' r = RealSrcSpan r Nothing
#endif
{-# COMPLETE RealSrcSpan', UnhelpfulSpan #-}

composeSrcSpan :: a -> a
composeSrcSpan = id

decomposeSrcSpan :: a -> a
decomposeSrcSpan = id

type SrcSpanLess a = a

type AnnSpan = RealSrcSpan
badAnnSpan :: AnnSpan
badAnnSpan =
  badRealSrcSpan

srcSpanToAnnSpan :: SrcSpan -> AnnSpan
srcSpanToAnnSpan =
  \case RealSrcSpan l _ -> l; _ -> badRealSrcSpan

annSpanToSrcSpan :: AnnSpan -> SrcSpan
annSpanToSrcSpan =
#if MIN_VERSION_ghc(9,4,0)
  flip RealSrcSpan Strict.Nothing
#else
  flip RealSrcSpan Nothing
#endif

setSrcSpanFile :: FastString -> SrcSpan -> SrcSpan
setSrcSpanFile file s
  | RealSrcLoc' start <- srcSpanStart s,
    RealSrcLoc' end <- srcSpanEnd s =
    let start' = mkSrcLoc file (srcLocLine start) (srcLocCol start)
        end' = mkSrcLoc file (srcLocLine end) (srcLocCol end)
     in mkSrcSpan start' end'
setSrcSpanFile _ s = s

setRealSrcSpanFile :: FastString -> RealSrcSpan -> RealSrcSpan
setRealSrcSpanFile file s = mkRealSrcSpan start' end'
  where
    start = realSrcSpanStart s
    end = realSrcSpanEnd s
    start' = mkRealSrcLoc file (srcLocLine start) (srcLocCol start)
    end' = mkRealSrcLoc file (srcLocLine end) (srcLocCol end)

setAnnSpanFile :: FastString -> AnnSpan -> AnnSpan
setAnnSpanFile =
  setRealSrcSpanFile

mkErr :: DynFlags -> SrcSpan -> String -> Errors
#if MIN_VERSION_ghc(9,4,0)
mkErr _df l s =
  mkMessages $
    unitBag (mkPlainErrorMsgEnvelope l (ghcUnknownMessage $ mkDecoratedError [] [text s]))
#else
mkErr _df l s = unitBag (mkPlainMsgEnvelope l (text s))
#endif

parseModuleName :: SrcSpan -> Parser (LocatedA GHC.ModuleName)
parseModuleName ss _ _ s =
  let newMN =  GHC.L (GHC.noAnnSrcSpan ss) (GHC.mkModuleName s)
  in pure newMN

type DoGenReplacement an ast a =
  (Data ast, Data a) =>
  a ->
  (LocatedAn an ast -> Bool) ->
  LocatedAn an ast ->
  LocatedAn an ast ->
  StateT Bool IO (LocatedAn an ast)

type ReplaceWorker a mod =
  (Data a, Data mod) =>
  mod ->
  Parser (GHC.LocatedA a) ->
  Int ->
  Refactoring SrcSpan ->
  IO mod


commentSrcSpan :: GHC.LEpaComment -> SrcSpan
#if MIN_VERSION_ghc(9,12,0)
commentSrcSpan (GHC.L (GHC.EpaSpan l) _) = l
commentSrcSpan (GHC.L (GHC.EpaDelta l _ _) _) = l
#elif MIN_VERSION_ghc(9,4,0)
commentSrcSpan (GHC.L (GHC.Anchor l _) _) = GHC.RealSrcSpan l Strict.Nothing
#else
commentSrcSpan (GHC.L (GHC.Anchor l _) _) = GHC.RealSrcSpan l Nothing
#endif

#if MIN_VERSION_ghc(9,12,0)
transferEntryDP :: (Typeable t1, Typeable t2, Exact.HasTransform m)
  => LocatedAn t1 a -> LocatedAn t2 b -> m (LocatedAn t2 b)
transferEntryDP a b = return $ Exact.transferEntryDP a b
#endif

#if MIN_VERSION_ghc(9,12,0)
transferEntryDP' ::(Exact.HasTransform m)
    => LHsDecl GhcPs -> LHsDecl GhcPs -> m (LHsDecl GhcPs)
transferEntryDP' a b = return $ Exact.transferEntryDP' a b
#endif


#if MIN_VERSION_ghc(9,12,0)
ann :: EpAnn a -> a
ann ls = GHC.anns ls
#else
ann :: SrcSpanAnn' a -> a
ann = GHC.ann
#endif

#if MIN_VERSION_ghc(9,6,0)
#else
data OnOff a = On a | Off a
#endif

#if MIN_VERSION_ghc(9,14,0)
#else
impliedXFlags :: [(Extension, OnOff Extension)]
impliedXFlags =
  [ (flag, if turnOn then On implied else Off implied)
  | (flag, turnOn, implied) <- Session.impliedXFlags
  ]
#endif

-- | The @LANGUAGE@ and @OPTIONS_GHC@ pragmas of a source file. Parse
-- diagnostics returned alongside them are discarded.
getOptions :: DynFlags -> StringBuffer -> FilePath -> [Located String]
#if MIN_VERSION_ghc(9,14,0)
getOptions flags buf fp =
  snd $ Header.getOptions (initParserOpts flags) (supportedLanguagePragmas flags) buf fp
#elif MIN_VERSION_ghc(9,4,0)
getOptions flags buf fp = snd $ Header.getOptions (initParserOpts flags) buf fp
#else
getOptions = Header.getOptions
#endif

-- | Apply a source file's pragma options to the flags. The unconsumed
-- arguments and warnings are discarded.
parseDynamicFilePragma :: DynFlags -> [Located String] -> IO DynFlags
parseDynamicFilePragma flags opts = do
#if MIN_VERSION_ghc(9,14,0)
  logger <- initLogger
  (flags', _, _) <- Session.parseDynamicFilePragma logger flags opts
#else
  (flags', _, _) <- Session.parseDynamicFilePragma flags opts
#endif
  pure flags'
