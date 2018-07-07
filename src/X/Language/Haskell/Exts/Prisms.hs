{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module X.Language.Haskell.Exts.Prisms where

import           Refactorio.Prelude                              hiding ( Alt )

import           Control.Lens                                           ( Iso'
                                                                        , Prism'
                                                                        , prism
                                                                        )
import           Data.ByteString.Char8                as Char8
import           Language.Haskell.Exts
import qualified Language.Haskell.Exts.Prisms         as X
import           Refactorio.Conversions                                 ( convert )
import           X.Language.Haskell.Exts.Prisms.Types as Exports

-- TODO: should change it to be a prism from (FilePath, ByteString) so we can
--       pass the filepath to parseMode to get meaningful error messags.
--       ... will have to modify the driver to pass the path and modify
--       everything else to expect it.
_Hask :: Prism' ByteString (Module SrcSpanInfo, [Comment])
_Hask = prism get_ set_
  where
    get_ :: (Module SrcSpanInfo, [Comment]) -> ByteString
    get_ = Char8.pack . uncurry exactPrint

    set_ :: ByteString -> Either ByteString (Module SrcSpanInfo, [Comment])
    set_ bs = case parseWithComments parseMode . Char8.unpack $ bs of
                ParseOk modWithComments -> Right modWithComments
                ParseFailed srcLoc' msg -> panic . show $ (msg,srcLoc',bs)
                -- ParseFailed _ _         -> Left bs
      where
        parseMode = (configureParseMode foundExtensions) { parseFilename = path}

        foundExtensions :: [Extension]
        foundExtensions = extractExtensions . view convert $ bs

        path = "_Hask Prism contents"

extractExtensions :: String -> [Extension]
extractExtensions = (EnableExtension MultiParamTypeClasses:) . unpackExts . readExtensions
  -- We append MultiParamTypeClasses because haskell-src-exts seems to need it
  -- sometimes even when nothing else does, and it should never hurt anything.
  where
    unpackExts :: Maybe (Maybe Language, [Extension]) -> [Extension]
    unpackExts = \case
      Nothing -> panic "could not parse language pragmas"
      Just (_, exts) -> exts

-- TODO: unhardcode
configureParseMode :: [Extension] -> ParseMode
configureParseMode foundExtensions = defaultParseMode
  { baseLanguage          = Haskell2010
  , ignoreLanguagePragmas = False
  , extensions            = configuredExtensions
  }
  where
    configuredExtensions = extensions defaultParseMode ++ foundExtensions

-- TODO: Surely there's a way to avoid the 'ambiguous l' problem without
-- duplicating all this code?
-- ================================================================================

_ParenFormula :: Prism'
                 (BooleanFormula SrcSpanInfo)
                 (SrcSpanInfo, BooleanFormula SrcSpanInfo)
_ParenFormula = X._ParenFormula

_OrFormula :: Prism' (BooleanFormula SrcSpanInfo) (SrcSpanInfo, [BooleanFormula SrcSpanInfo])
_OrFormula = X._OrFormula

_AndFormula :: Prism' (BooleanFormula SrcSpanInfo) (SrcSpanInfo, [BooleanFormula SrcSpanInfo])
_AndFormula = X._AndFormula

_VarFormula :: Prism' (BooleanFormula SrcSpanInfo) (SrcSpanInfo, Name SrcSpanInfo)
_VarFormula = X._VarFormula

_ModuleAnn :: Prism' (Annotation SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo)
_ModuleAnn = X._ModuleAnn

_TypeAnn :: Prism' (Annotation SrcSpanInfo) (SrcSpanInfo, Name SrcSpanInfo, Exp SrcSpanInfo)
_TypeAnn = X._TypeAnn

_Ann :: Prism' (Annotation SrcSpanInfo) (SrcSpanInfo, Name SrcSpanInfo, Exp SrcSpanInfo)
_Ann = X._Ann

_ActiveUntil :: Prism' (Activation SrcSpanInfo) (SrcSpanInfo, Int)
_ActiveUntil = X._ActiveUntil

_ActiveFrom :: Prism' (Activation SrcSpanInfo) (SrcSpanInfo, Int)
_ActiveFrom = X._ActiveFrom

_TypedRuleVar :: Prism' (RuleVar SrcSpanInfo) (SrcSpanInfo, Name SrcSpanInfo, Type SrcSpanInfo)
_TypedRuleVar = X._TypedRuleVar

_RuleVar :: Prism' (RuleVar SrcSpanInfo) (SrcSpanInfo, Name SrcSpanInfo)
_RuleVar = X._RuleVar

_Overlap :: Prism' (Overlap SrcSpanInfo) SrcSpanInfo
_Overlap = X._Overlap

_NoOverlap :: Prism' (Overlap SrcSpanInfo) SrcSpanInfo
_NoOverlap = X._NoOverlap

_UnknownTool :: Prism' Tool String
_UnknownTool = X._UnknownTool

_HADDOCK :: Prism' Tool ()
_HADDOCK = X._HADDOCK

_YHC :: Prism' Tool ()
_YHC = X._YHC

_NHC98 :: Prism' Tool ()
_NHC98 = X._NHC98

_HUGS :: Prism' Tool ()
_HUGS = X._HUGS

_GHC :: Prism' Tool ()
_GHC = X._GHC

_AnnModulePragma :: Prism' (ModulePragma SrcSpanInfo) (SrcSpanInfo, Annotation SrcSpanInfo)
_AnnModulePragma = X._AnnModulePragma

_OptionsPragma :: Prism' (ModulePragma SrcSpanInfo) (SrcSpanInfo, Maybe Tool, String)
_OptionsPragma = X._OptionsPragma

_LanguagePragma :: Prism' (ModulePragma SrcSpanInfo) (SrcSpanInfo, [Name SrcSpanInfo])
_LanguagePragma = X._LanguagePragma

_CApi :: Prism' (CallConv SrcSpanInfo) SrcSpanInfo
_CApi = X._CApi

_JavaScript :: Prism' (CallConv SrcSpanInfo) SrcSpanInfo
_JavaScript = X._JavaScript

_Js :: Prism' (CallConv SrcSpanInfo) SrcSpanInfo
_Js = X._Js

_Jvm :: Prism' (CallConv SrcSpanInfo) SrcSpanInfo
_Jvm = X._Jvm

_DotNet :: Prism' (CallConv SrcSpanInfo) SrcSpanInfo
_DotNet = X._DotNet

_CPlusPlus :: Prism' (CallConv SrcSpanInfo) SrcSpanInfo
_CPlusPlus = X._CPlusPlus

_CCall :: Prism' (CallConv SrcSpanInfo) SrcSpanInfo
_CCall = X._CCall

_StdCall :: Prism' (CallConv SrcSpanInfo) SrcSpanInfo
_StdCall = X._StdCall

_PlayInterruptible :: Prism' (Safety SrcSpanInfo) SrcSpanInfo
_PlayInterruptible = X._PlayInterruptible

_PlaySafe :: Prism' (Safety SrcSpanInfo) (SrcSpanInfo, Bool)
_PlaySafe = X._PlaySafe

_PlayRisky :: Prism' (Safety SrcSpanInfo) SrcSpanInfo
_PlayRisky = X._PlayRisky

_ParenSplice :: Prism' (Splice SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo)
_ParenSplice = X._ParenSplice

_IdSplice :: Prism' (Splice SrcSpanInfo) (SrcSpanInfo, String)
_IdSplice = X._IdSplice

_DeclBracket :: Prism' (Bracket SrcSpanInfo) (SrcSpanInfo, [Decl SrcSpanInfo])
_DeclBracket = X._DeclBracket

_TypeBracket :: Prism' (Bracket SrcSpanInfo) (SrcSpanInfo, Type SrcSpanInfo)
_TypeBracket = X._TypeBracket

_PatBracket :: Prism' (Bracket SrcSpanInfo) (SrcSpanInfo, Pat SrcSpanInfo)
_PatBracket = X._PatBracket

_ExpBracket :: Prism' (Bracket SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo)
_ExpBracket = X._ExpBracket

_RoleWildcard :: Prism' (Role SrcSpanInfo) SrcSpanInfo
_RoleWildcard = X._RoleWildcard

_Phantom :: Prism' (Role SrcSpanInfo) SrcSpanInfo
_Phantom = X._Phantom

_Representational :: Prism' (Role SrcSpanInfo) SrcSpanInfo
_Representational = X._Representational

_Nominal :: Prism' (Role SrcSpanInfo) SrcSpanInfo
_Nominal = X._Nominal

_XDomName :: Prism' (XName SrcSpanInfo) (SrcSpanInfo, String, String)
_XDomName = X._XDomName

_XName :: Prism' (XName SrcSpanInfo) (SrcSpanInfo, String)
_XName = X._XName

_IPLin :: Prism' (IPName SrcSpanInfo) (SrcSpanInfo, String)
_IPLin = X._IPLin

_IPDup :: Prism' (IPName SrcSpanInfo) (SrcSpanInfo, String)
_IPDup = X._IPDup

_ConName :: Prism' (CName SrcSpanInfo) (SrcSpanInfo, Name SrcSpanInfo)
_ConName = X._ConName

_VarName :: Prism' (CName SrcSpanInfo) (SrcSpanInfo, Name SrcSpanInfo)
_VarName = X._VarName

_UnboxedSingleCon :: Prism' (SpecialCon SrcSpanInfo) SrcSpanInfo
_UnboxedSingleCon = X._UnboxedSingleCon

_Cons :: Prism' (SpecialCon SrcSpanInfo) SrcSpanInfo
_Cons = X._Cons

_TupleCon :: Prism' (SpecialCon SrcSpanInfo) (SrcSpanInfo, Boxed, Int)
_TupleCon = X._TupleCon

_FunCon :: Prism' (SpecialCon SrcSpanInfo) SrcSpanInfo
_FunCon = X._FunCon

_ListCon :: Prism' (SpecialCon SrcSpanInfo) SrcSpanInfo
_ListCon = X._ListCon

_UnitCon :: Prism' (SpecialCon SrcSpanInfo) SrcSpanInfo
_UnitCon = X._UnitCon

_ConOp :: Prism' (Op SrcSpanInfo) (SrcSpanInfo, Name SrcSpanInfo)
_ConOp = X._ConOp

_VarOp :: Prism' (Op SrcSpanInfo) (SrcSpanInfo, Name SrcSpanInfo)
_VarOp = X._VarOp

_QConOp :: Prism' (QOp SrcSpanInfo) (SrcSpanInfo, QName SrcSpanInfo)
_QConOp = X._QConOp

_QVarOp :: Prism' (QOp SrcSpanInfo) (SrcSpanInfo, QName SrcSpanInfo)
_QVarOp = X._QVarOp

_Symbol :: Prism' (Name SrcSpanInfo) (SrcSpanInfo, String)
_Symbol = X._Symbol

_Ident :: Prism' (Name SrcSpanInfo) (SrcSpanInfo, String)
_Ident = X._Ident

_Special :: Prism' (QName SrcSpanInfo) (SrcSpanInfo, SpecialCon SrcSpanInfo)
_Special = X._Special

_UnQual :: Prism' (QName SrcSpanInfo) (SrcSpanInfo, Name SrcSpanInfo)
_UnQual = X._UnQual

_Qual :: Prism' (QName SrcSpanInfo) (SrcSpanInfo, ModuleName SrcSpanInfo, Name SrcSpanInfo)
_Qual = X._Qual

_Negative :: Prism' (Sign SrcSpanInfo) SrcSpanInfo
_Negative = X._Negative

_Signless :: Prism' (Sign SrcSpanInfo) SrcSpanInfo
_Signless = X._Signless

_PrimString :: Prism' (Literal SrcSpanInfo) (SrcSpanInfo, String, String)
_PrimString = X._PrimString

_PrimChar :: Prism' (Literal SrcSpanInfo) (SrcSpanInfo, Char, String)
_PrimChar = X._PrimChar

_PrimDouble :: Prism' (Literal SrcSpanInfo) (SrcSpanInfo, Rational, String)
_PrimDouble = X._PrimDouble

_PrimFloat :: Prism' (Literal SrcSpanInfo) (SrcSpanInfo, Rational, String)
_PrimFloat = X._PrimFloat

_PrimWord :: Prism' (Literal SrcSpanInfo) (SrcSpanInfo, Integer, String)
_PrimWord = X._PrimWord

_PrimInt :: Prism' (Literal SrcSpanInfo) (SrcSpanInfo, Integer, String)
_PrimInt = X._PrimInt

_Frac :: Prism' (Literal SrcSpanInfo) (SrcSpanInfo, Rational, String)
_Frac = X._Frac

_Int :: Prism' (Literal SrcSpanInfo) (SrcSpanInfo, Integer, String)
_Int = X._Int

_String :: Prism' (Literal SrcSpanInfo) (SrcSpanInfo, String, String)
_String = X._String

_Char :: Prism' (Literal SrcSpanInfo) (SrcSpanInfo, Char, String)
_Char = X._Char

_RPOptG :: Prism' (RPatOp SrcSpanInfo) SrcSpanInfo
_RPOptG = X._RPOptG

_RPOpt :: Prism' (RPatOp SrcSpanInfo) SrcSpanInfo
_RPOpt = X._RPOpt

_RPPlusG :: Prism' (RPatOp SrcSpanInfo) SrcSpanInfo
_RPPlusG = X._RPPlusG

_RPPlus :: Prism' (RPatOp SrcSpanInfo) SrcSpanInfo
_RPPlus = X._RPPlus

_RPStarG :: Prism' (RPatOp SrcSpanInfo) SrcSpanInfo
_RPStarG = X._RPStarG

_RPStar :: Prism' (RPatOp SrcSpanInfo) SrcSpanInfo
_RPStar = X._RPStar

_RPPat :: Prism' (RPat SrcSpanInfo) (SrcSpanInfo, Pat SrcSpanInfo)
_RPPat = X._RPPat

_RPParen :: Prism' (RPat SrcSpanInfo) (SrcSpanInfo, RPat SrcSpanInfo)
_RPParen = X._RPParen

_RPAs :: Prism' (RPat SrcSpanInfo) (SrcSpanInfo, Name SrcSpanInfo, RPat SrcSpanInfo)
_RPAs = X._RPAs

_RPCAs :: Prism' (RPat SrcSpanInfo) (SrcSpanInfo, Name SrcSpanInfo, RPat SrcSpanInfo)
_RPCAs = X._RPCAs

_RPGuard :: Prism' (RPat SrcSpanInfo) (SrcSpanInfo, Pat SrcSpanInfo, [Stmt SrcSpanInfo])
_RPGuard = X._RPGuard

_RPSeq :: Prism' (RPat SrcSpanInfo) (SrcSpanInfo, [RPat SrcSpanInfo])
_RPSeq = X._RPSeq

_RPEither :: Prism' (RPat SrcSpanInfo) (SrcSpanInfo, RPat SrcSpanInfo, RPat SrcSpanInfo)
_RPEither = X._RPEither

_RPOp :: Prism' (RPat SrcSpanInfo) (SrcSpanInfo, RPat SrcSpanInfo, RPatOp SrcSpanInfo)
_RPOp = X._RPOp

_PFieldWildcard :: Prism' (PatField SrcSpanInfo) SrcSpanInfo
_PFieldWildcard = X._PFieldWildcard

_PFieldPun :: Prism' (PatField SrcSpanInfo) (SrcSpanInfo, QName SrcSpanInfo)
_PFieldPun = X._PFieldPun

_PFieldPat :: Prism' (PatField SrcSpanInfo) (SrcSpanInfo, QName SrcSpanInfo, Pat SrcSpanInfo)
_PFieldPat = X._PFieldPat

_PBangPat :: Prism' (Pat SrcSpanInfo) (SrcSpanInfo, Pat SrcSpanInfo)
_PBangPat = X._PBangPat

_PQuasiQuote :: Prism' (Pat SrcSpanInfo) (SrcSpanInfo, String, String)
_PQuasiQuote = X._PQuasiQuote

_PXRPats :: Prism' (Pat SrcSpanInfo) (SrcSpanInfo, [RPat SrcSpanInfo])
_PXRPats = X._PXRPats

_PXPatTag :: Prism' (Pat SrcSpanInfo) (SrcSpanInfo, Pat SrcSpanInfo)
_PXPatTag = X._PXPatTag

_PXPcdata :: Prism' (Pat SrcSpanInfo) (SrcSpanInfo, String)
_PXPcdata = X._PXPcdata

_PXETag :: Prism' (Pat SrcSpanInfo) (SrcSpanInfo, XName SrcSpanInfo, [PXAttr SrcSpanInfo], Maybe (Pat SrcSpanInfo))
_PXETag = X._PXETag

_PXTag :: Prism' (Pat SrcSpanInfo) (SrcSpanInfo, XName SrcSpanInfo, [PXAttr SrcSpanInfo], Maybe (Pat SrcSpanInfo), [Pat SrcSpanInfo])
_PXTag = X._PXTag

_PRPat :: Prism' (Pat SrcSpanInfo) (SrcSpanInfo, [RPat SrcSpanInfo])
_PRPat = X._PRPat

_PViewPat :: Prism' (Pat SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo, Pat SrcSpanInfo)
_PViewPat = X._PViewPat

_PatTypeSig :: Prism' (Pat SrcSpanInfo) (SrcSpanInfo, Pat SrcSpanInfo, Type SrcSpanInfo)
_PatTypeSig = X._PatTypeSig

_PIrrPat :: Prism' (Pat SrcSpanInfo) (SrcSpanInfo, Pat SrcSpanInfo)
_PIrrPat = X._PIrrPat

_PWildCard :: Prism' (Pat SrcSpanInfo) SrcSpanInfo
_PWildCard = X._PWildCard

_PAsPat :: Prism' (Pat SrcSpanInfo) (SrcSpanInfo, Name SrcSpanInfo, Pat SrcSpanInfo)
_PAsPat = X._PAsPat

_PRec :: Prism' (Pat SrcSpanInfo) (SrcSpanInfo, QName SrcSpanInfo, [PatField SrcSpanInfo])
_PRec = X._PRec

_PParen :: Prism' (Pat SrcSpanInfo) (SrcSpanInfo, Pat SrcSpanInfo)
_PParen = X._PParen

_PList :: Prism' (Pat SrcSpanInfo) (SrcSpanInfo, [Pat SrcSpanInfo])
_PList = X._PList

_PTuple :: Prism' (Pat SrcSpanInfo) (SrcSpanInfo, Boxed, [Pat SrcSpanInfo])
_PTuple = X._PTuple

_PApp :: Prism' (Pat SrcSpanInfo) (SrcSpanInfo, QName SrcSpanInfo, [Pat SrcSpanInfo])
_PApp = X._PApp

_PInfixApp :: Prism' (Pat SrcSpanInfo) (SrcSpanInfo, Pat SrcSpanInfo, QName SrcSpanInfo, Pat SrcSpanInfo)
_PInfixApp = X._PInfixApp

_PNPlusK :: Prism' (Pat SrcSpanInfo) (SrcSpanInfo, Name SrcSpanInfo, Integer)
_PNPlusK = X._PNPlusK

_PLit :: Prism' (Pat SrcSpanInfo) (SrcSpanInfo, Sign SrcSpanInfo, Literal SrcSpanInfo)
_PLit = X._PLit

_PVar :: Prism' (Pat SrcSpanInfo) (SrcSpanInfo, Name SrcSpanInfo)
_PVar = X._PVar

_FieldWildcard :: Prism' (FieldUpdate SrcSpanInfo) SrcSpanInfo
_FieldWildcard = X._FieldWildcard

_FieldPun :: Prism' (FieldUpdate SrcSpanInfo) (SrcSpanInfo, QName SrcSpanInfo)
_FieldPun = X._FieldPun

_FieldUpdate :: Prism' (FieldUpdate SrcSpanInfo) (SrcSpanInfo, QName SrcSpanInfo, Exp SrcSpanInfo)
_FieldUpdate = X._FieldUpdate

_GroupByUsing :: Prism' (QualStmt SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo, Exp SrcSpanInfo)
_GroupByUsing = X._GroupByUsing

_GroupUsing :: Prism' (QualStmt SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo)
_GroupUsing = X._GroupUsing

_GroupBy :: Prism' (QualStmt SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo)
_GroupBy = X._GroupBy

_ThenBy :: Prism' (QualStmt SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo, Exp SrcSpanInfo)
_ThenBy = X._ThenBy

_ThenTrans :: Prism' (QualStmt SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo)
_ThenTrans = X._ThenTrans

_QualStmt :: Prism' (QualStmt SrcSpanInfo) (SrcSpanInfo, Stmt SrcSpanInfo)
_QualStmt = X._QualStmt

_RecStmt :: Prism' (Stmt SrcSpanInfo) (SrcSpanInfo, [Stmt SrcSpanInfo])
_RecStmt = X._RecStmt

_LetStmt :: Prism' (Stmt SrcSpanInfo) (SrcSpanInfo, Binds SrcSpanInfo)
_LetStmt = X._LetStmt

_Qualifier :: Prism' (Stmt SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo)
_Qualifier = X._Qualifier

_Generator :: Prism' (Stmt SrcSpanInfo) (SrcSpanInfo, Pat SrcSpanInfo, Exp SrcSpanInfo)
_Generator = X._Generator

_ExprHole :: Prism' (Exp SrcSpanInfo) SrcSpanInfo
_ExprHole = X._ExprHole

_LCase :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, [Alt SrcSpanInfo])
_LCase = X._LCase

_RightArrHighApp :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo, Exp SrcSpanInfo)
_RightArrHighApp = X._RightArrHighApp

_LeftArrHighApp :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo, Exp SrcSpanInfo)
_LeftArrHighApp = X._LeftArrHighApp

_RightArrApp :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo, Exp SrcSpanInfo)
_RightArrApp = X._RightArrApp

_LeftArrApp :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo, Exp SrcSpanInfo)
_LeftArrApp = X._LeftArrApp

_Proc :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, Pat SrcSpanInfo, Exp SrcSpanInfo)
_Proc = X._Proc

_GenPragma :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, String, (Int, Int), (Int, Int), Exp SrcSpanInfo)
_GenPragma = X._GenPragma

_SCCPragma :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, String, Exp SrcSpanInfo)
_SCCPragma = X._SCCPragma

_CorePragma :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, String, Exp SrcSpanInfo)
_CorePragma = X._CorePragma

_XChildTag :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, [Exp SrcSpanInfo])
_XChildTag = X._XChildTag

_XExpTag :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo)
_XExpTag = X._XExpTag

_XPcdata :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, String)
_XPcdata = X._XPcdata

_XETag :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, XName SrcSpanInfo, [XAttr SrcSpanInfo], Maybe (Exp SrcSpanInfo))
_XETag = X._XETag

_XTag :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, XName SrcSpanInfo, [XAttr SrcSpanInfo], Maybe (Exp SrcSpanInfo), [Exp SrcSpanInfo])
_XTag = X._XTag

_TypeApp :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, Type SrcSpanInfo)
_TypeApp = X._TypeApp

_QuasiQuote :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, String, String)
_QuasiQuote = X._QuasiQuote

_SpliceExp :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, Splice SrcSpanInfo)
_SpliceExp = X._SpliceExp

_BracketExp :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, Bracket SrcSpanInfo)
_BracketExp = X._BracketExp

_TypQuote :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, QName SrcSpanInfo)
_TypQuote = X._TypQuote

_VarQuote :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, QName SrcSpanInfo)
_VarQuote = X._VarQuote

_ExpTypeSig :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo, Type SrcSpanInfo)
_ExpTypeSig = X._ExpTypeSig

_ParArrayComp :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo, [[QualStmt SrcSpanInfo]])
_ParArrayComp = X._ParArrayComp

_ParComp :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo, [[QualStmt SrcSpanInfo]])
_ParComp = X._ParComp

_ListComp :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo, [QualStmt SrcSpanInfo])
_ListComp = X._ListComp

_ParArrayFromThenTo :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo, Exp SrcSpanInfo, Exp SrcSpanInfo)
_ParArrayFromThenTo = X._ParArrayFromThenTo

_ParArrayFromTo :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo, Exp SrcSpanInfo)
_ParArrayFromTo = X._ParArrayFromTo

_EnumFromThenTo :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo, Exp SrcSpanInfo, Exp SrcSpanInfo)
_EnumFromThenTo = X._EnumFromThenTo

_EnumFromThen :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo, Exp SrcSpanInfo)
_EnumFromThen = X._EnumFromThen

_EnumFromTo :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo, Exp SrcSpanInfo)
_EnumFromTo = X._EnumFromTo

_EnumFrom :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo)
_EnumFrom = X._EnumFrom

_RecUpdate :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo, [FieldUpdate SrcSpanInfo])
_RecUpdate = X._RecUpdate

_RecConstr :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, QName SrcSpanInfo, [FieldUpdate SrcSpanInfo])
_RecConstr = X._RecConstr

_RightSection :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, QOp SrcSpanInfo, Exp SrcSpanInfo)
_RightSection = X._RightSection

_LeftSection :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo, QOp SrcSpanInfo)
_LeftSection = X._LeftSection

_Paren :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo)
_Paren = X._Paren

_ParArray :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, [Exp SrcSpanInfo])
_ParArray = X._ParArray

_List :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, [Exp SrcSpanInfo])
_List = X._List

_TupleSection :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, Boxed, [Maybe (Exp SrcSpanInfo)])
_TupleSection = X._TupleSection

_Tuple :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, Boxed, [Exp SrcSpanInfo])
_Tuple = X._Tuple

_MDo :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, [Stmt SrcSpanInfo])
_MDo = X._MDo

_Do :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, [Stmt SrcSpanInfo])
_Do = X._Do

_Case :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo, [Alt SrcSpanInfo])
_Case = X._Case

_MultiIf :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, [GuardedRhs SrcSpanInfo])
_MultiIf = X._MultiIf

_If :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo, Exp SrcSpanInfo, Exp SrcSpanInfo)
_If = X._If

_Let :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, Binds SrcSpanInfo, Exp SrcSpanInfo)
_Let = X._Let

_Lambda :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, [Pat SrcSpanInfo], Exp SrcSpanInfo)
_Lambda = X._Lambda

_NegApp :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo)
_NegApp = X._NegApp

_App :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo, Exp SrcSpanInfo)
_App = X._App

_InfixApp :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo, QOp SrcSpanInfo, Exp SrcSpanInfo)
_InfixApp = X._InfixApp

_Lit :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, Literal SrcSpanInfo)
_Lit = X._Lit

_Con :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, QName SrcSpanInfo)
_Con = X._Con

_IPVar :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, IPName SrcSpanInfo)
_IPVar = X._IPVar

_OverloadedLabel :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, String)
_OverloadedLabel = X._OverloadedLabel

_Var :: Prism' (Exp SrcSpanInfo) (SrcSpanInfo, QName SrcSpanInfo)
_Var = X._Var

_PromotedUnit :: Prism' (Promoted SrcSpanInfo) SrcSpanInfo
_PromotedUnit = X._PromotedUnit

_PromotedTuple :: Prism' (Promoted SrcSpanInfo) (SrcSpanInfo, [Type SrcSpanInfo])
_PromotedTuple = X._PromotedTuple

_PromotedList :: Prism' (Promoted SrcSpanInfo) (SrcSpanInfo, Bool, [Type SrcSpanInfo])
_PromotedList = X._PromotedList

_PromotedCon :: Prism' (Promoted SrcSpanInfo) (SrcSpanInfo, Bool, QName SrcSpanInfo)
_PromotedCon = X._PromotedCon

_PromotedString :: Prism' (Promoted SrcSpanInfo) (SrcSpanInfo, String, String)
_PromotedString = X._PromotedString

_PromotedInteger :: Prism' (Promoted SrcSpanInfo) (SrcSpanInfo, Integer, String)
_PromotedInteger = X._PromotedInteger

_UnkindedVar :: Prism' (TyVarBind SrcSpanInfo) (SrcSpanInfo, Name SrcSpanInfo)
_UnkindedVar = X._UnkindedVar

_KindedVar :: Prism' (TyVarBind SrcSpanInfo) (SrcSpanInfo, Name SrcSpanInfo, Kind SrcSpanInfo)
_KindedVar = X._KindedVar

_KindList :: Prism' (Kind SrcSpanInfo) (SrcSpanInfo, Kind SrcSpanInfo)
_KindList = X._KindList

_KindTuple :: Prism' (Kind SrcSpanInfo) (SrcSpanInfo, [Kind SrcSpanInfo])
_KindTuple = X._KindTuple

_KindApp :: Prism' (Kind SrcSpanInfo) (SrcSpanInfo, Kind SrcSpanInfo, Kind SrcSpanInfo)
_KindApp = X._KindApp

_KindVar :: Prism' (Kind SrcSpanInfo) (SrcSpanInfo, QName SrcSpanInfo)
_KindVar = X._KindVar

_KindParen :: Prism' (Kind SrcSpanInfo) (SrcSpanInfo, Kind SrcSpanInfo)
_KindParen = X._KindParen

_KindFn :: Prism' (Kind SrcSpanInfo) (SrcSpanInfo, Kind SrcSpanInfo, Kind SrcSpanInfo)
_KindFn = X._KindFn

_KindStar :: Prism' (Kind SrcSpanInfo) SrcSpanInfo
_KindStar = X._KindStar

_Unboxed :: Prism' Boxed ()
_Unboxed = X._Unboxed

_Boxed :: Prism' Boxed ()
_Boxed = X._Boxed

_TyQuasiQuote :: Prism' (Type SrcSpanInfo) (SrcSpanInfo, String, String)
_TyQuasiQuote = X._TyQuasiQuote

_TyWildCard :: Prism' (Type SrcSpanInfo) (SrcSpanInfo, Maybe (Name SrcSpanInfo))
_TyWildCard = X._TyWildCard

_TyBang :: Prism' (Type SrcSpanInfo) (SrcSpanInfo, BangType SrcSpanInfo, Unpackedness SrcSpanInfo, Type SrcSpanInfo)
_TyBang = X._TyBang

_TySplice :: Prism' (Type SrcSpanInfo) (SrcSpanInfo, Splice SrcSpanInfo)
_TySplice = X._TySplice

_TyEquals :: Prism' (Type SrcSpanInfo) (SrcSpanInfo, Type SrcSpanInfo, Type SrcSpanInfo)
_TyEquals = X._TyEquals

_TyPromoted :: Prism' (Type SrcSpanInfo) (SrcSpanInfo, Promoted SrcSpanInfo)
_TyPromoted = X._TyPromoted

_TyKind :: Prism' (Type SrcSpanInfo) (SrcSpanInfo, Type SrcSpanInfo, Kind SrcSpanInfo)
_TyKind = X._TyKind

_TyInfix :: Prism' (Type SrcSpanInfo) (SrcSpanInfo, Type SrcSpanInfo, QName SrcSpanInfo, Type SrcSpanInfo)
_TyInfix = X._TyInfix

_TyParen :: Prism' (Type SrcSpanInfo) (SrcSpanInfo, Type SrcSpanInfo)
_TyParen = X._TyParen

_TyCon :: Prism' (Type SrcSpanInfo) (SrcSpanInfo, QName SrcSpanInfo)
_TyCon = X._TyCon

_TyVar :: Prism' (Type SrcSpanInfo) (SrcSpanInfo, Name SrcSpanInfo)
_TyVar = X._TyVar

_TyApp :: Prism' (Type SrcSpanInfo) (SrcSpanInfo, Type SrcSpanInfo, Type SrcSpanInfo)
_TyApp = X._TyApp

_TyParArray :: Prism' (Type SrcSpanInfo) (SrcSpanInfo, Type SrcSpanInfo)
_TyParArray = X._TyParArray

_TyList :: Prism' (Type SrcSpanInfo) (SrcSpanInfo, Type SrcSpanInfo)
_TyList = X._TyList

_TyTuple :: Prism' (Type SrcSpanInfo) (SrcSpanInfo, Boxed, [Type SrcSpanInfo])
_TyTuple = X._TyTuple

_TyFun :: Prism' (Type SrcSpanInfo) (SrcSpanInfo, Type SrcSpanInfo, Type SrcSpanInfo)
_TyFun = X._TyFun

_TyForall :: Prism' (Type SrcSpanInfo) (SrcSpanInfo, Maybe [TyVarBind SrcSpanInfo], Maybe (Context SrcSpanInfo), Type SrcSpanInfo)
_TyForall = X._TyForall

_WildCardA :: Prism' (Asst SrcSpanInfo) (SrcSpanInfo, Maybe (Name SrcSpanInfo))
_WildCardA = X._WildCardA

_ParenA :: Prism' (Asst SrcSpanInfo) (SrcSpanInfo, Asst SrcSpanInfo)
_ParenA = X._ParenA

_EqualP :: Prism' (Asst SrcSpanInfo) (SrcSpanInfo, Type SrcSpanInfo, Type SrcSpanInfo)
_EqualP = X._EqualP

_IParam :: Prism' (Asst SrcSpanInfo) (SrcSpanInfo, IPName SrcSpanInfo, Type SrcSpanInfo)
_IParam = X._IParam

_InfixA :: Prism' (Asst SrcSpanInfo) (SrcSpanInfo, Type SrcSpanInfo, QName SrcSpanInfo, Type SrcSpanInfo)
_InfixA = X._InfixA

_AppA :: Prism' (Asst SrcSpanInfo) (SrcSpanInfo, Name SrcSpanInfo, [Type SrcSpanInfo])
_AppA = X._AppA

_ClassA :: Prism' (Asst SrcSpanInfo) (SrcSpanInfo, QName SrcSpanInfo, [Type SrcSpanInfo])
_ClassA = X._ClassA

_CxEmpty :: Prism' (Context SrcSpanInfo) SrcSpanInfo
_CxEmpty = X._CxEmpty

_CxTuple :: Prism' (Context SrcSpanInfo) (SrcSpanInfo, [Asst SrcSpanInfo])
_CxTuple = X._CxTuple

_CxSingle :: Prism' (Context SrcSpanInfo) (SrcSpanInfo, Asst SrcSpanInfo)
_CxSingle = X._CxSingle

_GuardedRhss :: Prism' (Rhs SrcSpanInfo) (SrcSpanInfo, [GuardedRhs SrcSpanInfo])
_GuardedRhss = X._GuardedRhss

_UnGuardedRhs :: Prism' (Rhs SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo)
_UnGuardedRhs = X._UnGuardedRhs

_InfixMatch :: Prism' (Match SrcSpanInfo) (SrcSpanInfo, Pat SrcSpanInfo, Name SrcSpanInfo, [Pat SrcSpanInfo], Rhs SrcSpanInfo, Maybe (Binds SrcSpanInfo))
_InfixMatch = X._InfixMatch

_Match :: Prism' (Match SrcSpanInfo) (SrcSpanInfo, Name SrcSpanInfo, [Pat SrcSpanInfo], Rhs SrcSpanInfo, Maybe (Binds SrcSpanInfo))
_Match = X._Match

_NoUnpackPragma :: Prism' (Unpackedness SrcSpanInfo) SrcSpanInfo
_NoUnpackPragma = X._NoUnpackPragma

_NoUnpack :: Prism' (Unpackedness SrcSpanInfo) SrcSpanInfo
_NoUnpack = X._NoUnpack

_Unpack :: Prism' (Unpackedness SrcSpanInfo) SrcSpanInfo
_Unpack = X._Unpack

_NoStrictAnnot :: Prism' (BangType SrcSpanInfo) SrcSpanInfo
_NoStrictAnnot = X._NoStrictAnnot

_LazyTy :: Prism' (BangType SrcSpanInfo) SrcSpanInfo
_LazyTy = X._LazyTy

_BangedTy :: Prism' (BangType SrcSpanInfo) SrcSpanInfo
_BangedTy = X._BangedTy

_RecDecl :: Prism' (ConDecl SrcSpanInfo) (SrcSpanInfo, Name SrcSpanInfo, [FieldDecl SrcSpanInfo])
_RecDecl = X._RecDecl

_InfixConDecl :: Prism' (ConDecl SrcSpanInfo) (SrcSpanInfo, Type SrcSpanInfo, Name SrcSpanInfo, Type SrcSpanInfo)
_InfixConDecl = X._InfixConDecl

_ConDecl :: Prism' (ConDecl SrcSpanInfo) (SrcSpanInfo, Name SrcSpanInfo, [Type SrcSpanInfo])
_ConDecl = X._ConDecl

_NewType :: Prism' (DataOrNew SrcSpanInfo) SrcSpanInfo
_NewType = X._NewType

_DataType :: Prism' (DataOrNew SrcSpanInfo) SrcSpanInfo
_DataType = X._DataType

_InsGData :: Prism' (InstDecl SrcSpanInfo) (SrcSpanInfo, DataOrNew SrcSpanInfo, Type SrcSpanInfo, Maybe (Kind SrcSpanInfo), [GadtDecl SrcSpanInfo], Maybe (Deriving SrcSpanInfo))
_InsGData = X._InsGData

_InsData :: Prism' (InstDecl SrcSpanInfo) (SrcSpanInfo, DataOrNew SrcSpanInfo, Type SrcSpanInfo, [QualConDecl SrcSpanInfo], Maybe (Deriving SrcSpanInfo))
_InsData = X._InsData

_InsType :: Prism' (InstDecl SrcSpanInfo) (SrcSpanInfo, Type SrcSpanInfo, Type SrcSpanInfo)
_InsType = X._InsType

_InsDecl :: Prism' (InstDecl SrcSpanInfo) (SrcSpanInfo, Decl SrcSpanInfo)
_InsDecl = X._InsDecl

_ClsDefSig :: Prism' (ClassDecl SrcSpanInfo) (SrcSpanInfo, Name SrcSpanInfo, Type SrcSpanInfo)
_ClsDefSig = X._ClsDefSig

_ClsTyDef :: Prism' (ClassDecl SrcSpanInfo) (SrcSpanInfo, TypeEqn SrcSpanInfo)
_ClsTyDef = X._ClsTyDef

_ClsTyFam :: Prism' (ClassDecl SrcSpanInfo) (SrcSpanInfo, DeclHead SrcSpanInfo, Maybe (ResultSig SrcSpanInfo), Maybe (InjectivityInfo SrcSpanInfo))
_ClsTyFam = X._ClsTyFam

_ClsDataFam :: Prism' (ClassDecl SrcSpanInfo) (SrcSpanInfo, Maybe (Context SrcSpanInfo), DeclHead SrcSpanInfo, Maybe (ResultSig SrcSpanInfo))
_ClsDataFam = X._ClsDataFam

_ClsDecl :: Prism' (ClassDecl SrcSpanInfo) (SrcSpanInfo, Decl SrcSpanInfo)
_ClsDecl = X._ClsDecl

_TyVarSig :: Prism' (ResultSig SrcSpanInfo) (SrcSpanInfo, TyVarBind SrcSpanInfo)
_TyVarSig = X._TyVarSig

_KindSig :: Prism' (ResultSig SrcSpanInfo) (SrcSpanInfo, Kind SrcSpanInfo)
_KindSig = X._KindSig

_ImplicitBidirectional :: Prism' (PatternSynDirection SrcSpanInfo) ()
_ImplicitBidirectional = X._ImplicitBidirectional

_Unidirectional :: Prism' (PatternSynDirection SrcSpanInfo) ()
_Unidirectional = X._Unidirectional

_IPBinds :: Prism' (Binds SrcSpanInfo) (SrcSpanInfo, [IPBind SrcSpanInfo])
_IPBinds = X._IPBinds

_BDecls :: Prism' (Binds SrcSpanInfo) (SrcSpanInfo, [Decl SrcSpanInfo])
_BDecls = X._BDecls

_IHApp :: Prism' (InstHead SrcSpanInfo) (SrcSpanInfo, InstHead SrcSpanInfo, Type SrcSpanInfo)
_IHApp = X._IHApp

_IHParen :: Prism' (InstHead SrcSpanInfo) (SrcSpanInfo, InstHead SrcSpanInfo)
_IHParen = X._IHParen

_IHInfix :: Prism' (InstHead SrcSpanInfo) (SrcSpanInfo, Type SrcSpanInfo, QName SrcSpanInfo)
_IHInfix = X._IHInfix

_IHCon :: Prism' (InstHead SrcSpanInfo) (SrcSpanInfo, QName SrcSpanInfo)
_IHCon = X._IHCon

_IParen :: Prism' (InstRule SrcSpanInfo) (SrcSpanInfo, InstRule SrcSpanInfo)
_IParen = X._IParen

_IRule :: Prism' (InstRule SrcSpanInfo) (SrcSpanInfo, Maybe [TyVarBind SrcSpanInfo], Maybe (Context SrcSpanInfo), InstHead SrcSpanInfo)
_IRule = X._IRule

_DHApp :: Prism' (DeclHead SrcSpanInfo) (SrcSpanInfo, DeclHead SrcSpanInfo, TyVarBind SrcSpanInfo)
_DHApp = X._DHApp

_DHParen :: Prism' (DeclHead SrcSpanInfo) (SrcSpanInfo, DeclHead SrcSpanInfo)
_DHParen = X._DHParen

_DHInfix :: Prism' (DeclHead SrcSpanInfo) (SrcSpanInfo, TyVarBind SrcSpanInfo, Name SrcSpanInfo)
_DHInfix = X._DHInfix

_DHead :: Prism' (DeclHead SrcSpanInfo) (SrcSpanInfo, Name SrcSpanInfo)
_DHead = X._DHead

_RoleAnnotDecl :: Prism' (Decl SrcSpanInfo) (SrcSpanInfo, QName SrcSpanInfo, [Role SrcSpanInfo])
_RoleAnnotDecl = X._RoleAnnotDecl

_MinimalPragma :: Prism' (Decl SrcSpanInfo) (SrcSpanInfo, Maybe (BooleanFormula SrcSpanInfo))
_MinimalPragma = X._MinimalPragma

_AnnPragma :: Prism' (Decl SrcSpanInfo) (SrcSpanInfo, Annotation SrcSpanInfo)
_AnnPragma = X._AnnPragma

_InstSig :: Prism' (Decl SrcSpanInfo) (SrcSpanInfo, InstRule SrcSpanInfo)
_InstSig = X._InstSig

_SpecInlineSig :: Prism' (Decl SrcSpanInfo) (SrcSpanInfo, Bool, Maybe (Activation SrcSpanInfo), QName SrcSpanInfo, [Type SrcSpanInfo])
_SpecInlineSig = X._SpecInlineSig

_SpecSig :: Prism' (Decl SrcSpanInfo) (SrcSpanInfo, Maybe (Activation SrcSpanInfo), QName SrcSpanInfo, [Type SrcSpanInfo])
_SpecSig = X._SpecSig

_InlineConlikeSig :: Prism' (Decl SrcSpanInfo) (SrcSpanInfo, Maybe (Activation SrcSpanInfo), QName SrcSpanInfo)
_InlineConlikeSig = X._InlineConlikeSig

_InlineSig :: Prism' (Decl SrcSpanInfo) (SrcSpanInfo, Bool, Maybe (Activation SrcSpanInfo), QName SrcSpanInfo)
_InlineSig = X._InlineSig

_WarnPragmaDecl :: Prism' (Decl SrcSpanInfo) (SrcSpanInfo, [([Name SrcSpanInfo], String)])
_WarnPragmaDecl = X._WarnPragmaDecl

_DeprPragmaDecl :: Prism' (Decl SrcSpanInfo) (SrcSpanInfo, [([Name SrcSpanInfo], String)])
_DeprPragmaDecl = X._DeprPragmaDecl

_RulePragmaDecl :: Prism' (Decl SrcSpanInfo) (SrcSpanInfo, [Rule SrcSpanInfo])
_RulePragmaDecl = X._RulePragmaDecl

_ForExp :: Prism' (Decl SrcSpanInfo) (SrcSpanInfo, CallConv SrcSpanInfo, Maybe String, Name SrcSpanInfo, Type SrcSpanInfo)
_ForExp = X._ForExp

_ForImp :: Prism' (Decl SrcSpanInfo) (SrcSpanInfo, CallConv SrcSpanInfo, Maybe (Safety SrcSpanInfo), Maybe String, Name SrcSpanInfo, Type SrcSpanInfo)
_ForImp = X._ForImp

_PatSyn :: Prism' (Decl SrcSpanInfo) (SrcSpanInfo, Pat SrcSpanInfo, Pat SrcSpanInfo, PatternSynDirection SrcSpanInfo)
_PatSyn = X._PatSyn

_PatBind :: Prism' (Decl SrcSpanInfo) (SrcSpanInfo, Pat SrcSpanInfo, Rhs SrcSpanInfo, Maybe (Binds SrcSpanInfo))
_PatBind = X._PatBind

_FunBind :: Prism' (Decl SrcSpanInfo) (SrcSpanInfo, [Match SrcSpanInfo])
_FunBind = X._FunBind

_PatSynSig :: Prism' (Decl SrcSpanInfo) (SrcSpanInfo, Name SrcSpanInfo, Maybe [TyVarBind SrcSpanInfo], Maybe (Context SrcSpanInfo), Maybe (Context SrcSpanInfo), Type SrcSpanInfo)
_PatSynSig = X._PatSynSig

_TypeSig :: Prism' (Decl SrcSpanInfo) (SrcSpanInfo, [Name SrcSpanInfo], Type SrcSpanInfo)
_TypeSig = X._TypeSig

_SpliceDecl :: Prism' (Decl SrcSpanInfo) (SrcSpanInfo, Exp SrcSpanInfo)
_SpliceDecl = X._SpliceDecl

_DefaultDecl :: Prism' (Decl SrcSpanInfo) (SrcSpanInfo, [Type SrcSpanInfo])
_DefaultDecl = X._DefaultDecl

_InfixDecl :: Prism' (Decl SrcSpanInfo) (SrcSpanInfo, Assoc SrcSpanInfo, Maybe Int, [Op SrcSpanInfo])
_InfixDecl = X._InfixDecl

_DerivDecl :: Prism' (Decl SrcSpanInfo) (SrcSpanInfo, Maybe (Overlap SrcSpanInfo), InstRule SrcSpanInfo)
_DerivDecl = X._DerivDecl

_InstDecl :: Prism' (Decl SrcSpanInfo) (SrcSpanInfo, Maybe (Overlap SrcSpanInfo), InstRule SrcSpanInfo, Maybe [InstDecl SrcSpanInfo])
_InstDecl = X._InstDecl

_ClassDecl :: Prism' (Decl SrcSpanInfo) (SrcSpanInfo, Maybe (Context SrcSpanInfo), DeclHead SrcSpanInfo, [FunDep SrcSpanInfo], Maybe [ClassDecl SrcSpanInfo])
_ClassDecl = X._ClassDecl

_GDataInsDecl :: Prism' (Decl SrcSpanInfo) (SrcSpanInfo, DataOrNew SrcSpanInfo, Type SrcSpanInfo, Maybe (Kind SrcSpanInfo), [GadtDecl SrcSpanInfo], Maybe (Deriving SrcSpanInfo))
_GDataInsDecl = X._GDataInsDecl

_DataInsDecl :: Prism' (Decl SrcSpanInfo) (SrcSpanInfo, DataOrNew SrcSpanInfo, Type SrcSpanInfo, [QualConDecl SrcSpanInfo], Maybe (Deriving SrcSpanInfo))
_DataInsDecl = X._DataInsDecl

_TypeInsDecl :: Prism' (Decl SrcSpanInfo) (SrcSpanInfo, Type SrcSpanInfo, Type SrcSpanInfo)
_TypeInsDecl = X._TypeInsDecl

_DataFamDecl :: Prism' (Decl SrcSpanInfo) (SrcSpanInfo, Maybe (Context SrcSpanInfo), DeclHead SrcSpanInfo, Maybe (ResultSig SrcSpanInfo))
_DataFamDecl = X._DataFamDecl

_GDataDecl :: Prism' (Decl SrcSpanInfo) (SrcSpanInfo, DataOrNew SrcSpanInfo, Maybe (Context SrcSpanInfo), DeclHead SrcSpanInfo, Maybe (Kind SrcSpanInfo), [GadtDecl SrcSpanInfo], Maybe (Deriving SrcSpanInfo))
_GDataDecl = X._GDataDecl

_DataDecl :: Prism' (Decl SrcSpanInfo) (SrcSpanInfo, DataOrNew SrcSpanInfo, Maybe (Context SrcSpanInfo), DeclHead SrcSpanInfo, [QualConDecl SrcSpanInfo], Maybe (Deriving SrcSpanInfo))
_DataDecl = X._DataDecl

_ClosedTypeFamDecl :: Prism' (Decl SrcSpanInfo) (SrcSpanInfo, DeclHead SrcSpanInfo, Maybe (ResultSig SrcSpanInfo), Maybe (InjectivityInfo SrcSpanInfo), [TypeEqn SrcSpanInfo])
_ClosedTypeFamDecl = X._ClosedTypeFamDecl

_TypeFamDecl :: Prism' (Decl SrcSpanInfo) (SrcSpanInfo, DeclHead SrcSpanInfo, Maybe (ResultSig SrcSpanInfo), Maybe (InjectivityInfo SrcSpanInfo))
_TypeFamDecl = X._TypeFamDecl

_TypeDecl :: Prism' (Decl SrcSpanInfo) (SrcSpanInfo, DeclHead SrcSpanInfo, Type SrcSpanInfo)
_TypeDecl = X._TypeDecl

_PatternNamespace :: Prism' (Namespace SrcSpanInfo) SrcSpanInfo
_PatternNamespace = X._PatternNamespace

_TypeNamespace :: Prism' (Namespace SrcSpanInfo) SrcSpanInfo
_TypeNamespace = X._TypeNamespace

_NoNamespace :: Prism' (Namespace SrcSpanInfo) SrcSpanInfo
_NoNamespace = X._NoNamespace

_AssocRight :: Prism' (Assoc SrcSpanInfo) SrcSpanInfo
_AssocRight = X._AssocRight

_AssocLeft :: Prism' (Assoc SrcSpanInfo) SrcSpanInfo
_AssocLeft = X._AssocLeft

_AssocNone :: Prism' (Assoc SrcSpanInfo) SrcSpanInfo
_AssocNone = X._AssocNone

_IThingWith :: Prism' (ImportSpec SrcSpanInfo) (SrcSpanInfo, Name SrcSpanInfo, [CName SrcSpanInfo])
_IThingWith = X._IThingWith

_IThingAll :: Prism' (ImportSpec SrcSpanInfo) (SrcSpanInfo, Name SrcSpanInfo)
_IThingAll = X._IThingAll

_IAbs :: Prism' (ImportSpec SrcSpanInfo) (SrcSpanInfo, Namespace SrcSpanInfo, Name SrcSpanInfo)
_IAbs = X._IAbs

_IVar :: Prism' (ImportSpec SrcSpanInfo) (SrcSpanInfo, Name SrcSpanInfo)
_IVar = X._IVar

_EWildcard :: Prism' (EWildcard SrcSpanInfo) (SrcSpanInfo, Int)
_EWildcard = X._EWildcard

_NoWildcard :: Prism' (EWildcard SrcSpanInfo) SrcSpanInfo
_NoWildcard = X._NoWildcard

_EModuleContents :: Prism' (ExportSpec SrcSpanInfo) (SrcSpanInfo, ModuleName SrcSpanInfo)
_EModuleContents = X._EModuleContents

_EThingWith :: Prism' (ExportSpec SrcSpanInfo) (SrcSpanInfo, EWildcard SrcSpanInfo, QName SrcSpanInfo, [CName SrcSpanInfo])
_EThingWith = X._EThingWith

_EAbs :: Prism' (ExportSpec SrcSpanInfo) (SrcSpanInfo, Namespace SrcSpanInfo, QName SrcSpanInfo)
_EAbs = X._EAbs

_EVar :: Prism' (ExportSpec SrcSpanInfo) (SrcSpanInfo, QName SrcSpanInfo)
_EVar = X._EVar

_WarnText :: Prism' (WarningText SrcSpanInfo) (SrcSpanInfo, String)
_WarnText = X._WarnText

_DeprText :: Prism' (WarningText SrcSpanInfo) (SrcSpanInfo, String)
_DeprText = X._DeprText

_XmlHybrid :: Prism' (Module SrcSpanInfo) (SrcSpanInfo, Maybe (ModuleHead SrcSpanInfo), [ModulePragma SrcSpanInfo], [ImportDecl SrcSpanInfo], [Decl SrcSpanInfo], XName SrcSpanInfo, [XAttr SrcSpanInfo], Maybe (Exp SrcSpanInfo), [Exp SrcSpanInfo])
_XmlHybrid = X._XmlHybrid

_XmlPage :: Prism' (Module SrcSpanInfo) (SrcSpanInfo, ModuleName SrcSpanInfo, [ModulePragma SrcSpanInfo], XName SrcSpanInfo, [XAttr SrcSpanInfo], Maybe (Exp SrcSpanInfo), [Exp SrcSpanInfo])
_XmlPage = X._XmlPage

_Module :: Prism' (Module SrcSpanInfo) (SrcSpanInfo, Maybe (ModuleHead SrcSpanInfo), [ModulePragma SrcSpanInfo], [ImportDecl SrcSpanInfo], [Decl SrcSpanInfo])
_Module = X._Module

_ParenFormula' :: Prism' (BooleanFormula v) (C_ParenFormula v)
_ParenFormula' = X._ParenFormula'

_OrFormula' :: Prism' (BooleanFormula v) (C_OrFormula v)
_OrFormula' = X._OrFormula'

_AndFormula' :: Prism' (BooleanFormula v) (C_AndFormula v)
_AndFormula' = X._AndFormula'

_VarFormula' :: Prism' (BooleanFormula v) (C_VarFormula v)
_VarFormula' = X._VarFormula'

_ModuleAnn' :: Prism' (Annotation v) (C_ModuleAnn v)
_ModuleAnn' = X._ModuleAnn'

_TypeAnn' :: Prism' (Annotation v) (C_TypeAnn v)
_TypeAnn' = X._TypeAnn'

_Ann' :: Prism' (Annotation v) (C_Ann v)
_Ann' = X._Ann'

_ActiveUntil' :: Prism' (Activation v) (C_ActiveUntil v)
_ActiveUntil' = X._ActiveUntil'

_ActiveFrom' :: Prism' (Activation v) (C_ActiveFrom v)
_ActiveFrom' = X._ActiveFrom'

_TypedRuleVar' :: Prism' (RuleVar v) (C_TypedRuleVar v)
_TypedRuleVar' = X._TypedRuleVar'

_RuleVar' :: Prism' (RuleVar v) (C_RuleVar v)
_RuleVar' = X._RuleVar'

_Incoherent' :: Prism' (Overlap v) (C_Incoherent v)
_Incoherent' = X._Incoherent'

_Overlap' :: Prism' (Overlap v) (C_Overlap v)
_Overlap' = X._Overlap'

_NoOverlap' :: Prism' (Overlap v) (C_NoOverlap v)
_NoOverlap' = X._NoOverlap'

_UnknownTool' :: Prism' Tool C_UnknownTool
_UnknownTool' = X._UnknownTool'

_HADDOCK' :: Prism' Tool C_HADDOCK
_HADDOCK' = X._HADDOCK'

_YHC' :: Prism' Tool C_YHC
_YHC' = X._YHC'

_NHC98' :: Prism' Tool C_NHC98
_NHC98' = X._NHC98'

_HUGS' :: Prism' Tool C_HUGS
_HUGS' = X._HUGS'

_GHC' :: Prism' Tool C_GHC
_GHC' = X._GHC'

_AnnModulePragma' :: Prism' (ModulePragma v) (C_AnnModulePragma v)
_AnnModulePragma' = X._AnnModulePragma'

_OptionsPragma' :: Prism' (ModulePragma v) (C_OptionsPragma v)
_OptionsPragma' = X._OptionsPragma'

_LanguagePragma' :: Prism' (ModulePragma v) (C_LanguagePragma v)
_LanguagePragma' = X._LanguagePragma'

_CApi' :: Prism' (CallConv v) (C_CApi v)
_CApi' = X._CApi'

_JavaScript' :: Prism' (CallConv v) (C_JavaScript v)
_JavaScript' = X._JavaScript'

_Js' :: Prism' (CallConv v) (C_Js v)
_Js' = X._Js'

_Jvm' :: Prism' (CallConv v) (C_Jvm v)
_Jvm' = X._Jvm'

_DotNet' :: Prism' (CallConv v) (C_DotNet v)
_DotNet' = X._DotNet'

_CPlusPlus' :: Prism' (CallConv v) (C_CPlusPlus v)
_CPlusPlus' = X._CPlusPlus'

_CCall' :: Prism' (CallConv v) (C_CCall v)
_CCall' = X._CCall'

_StdCall' :: Prism' (CallConv v) (C_StdCall v)
_StdCall' = X._StdCall'

_PlayInterruptible' :: Prism' (Safety v) (C_PlayInterruptible v)
_PlayInterruptible' = X._PlayInterruptible'

_PlaySafe' :: Prism' (Safety v) (C_PlaySafe v)
_PlaySafe' = X._PlaySafe'

_PlayRisky' :: Prism' (Safety v) (C_PlayRisky v)
_PlayRisky' = X._PlayRisky'

_ParenSplice' :: Prism' (Splice v) (C_ParenSplice v)
_ParenSplice' = X._ParenSplice'

_IdSplice' :: Prism' (Splice v) (C_IdSplice v)
_IdSplice' = X._IdSplice'

_DeclBracket' :: Prism' (Bracket v) (C_DeclBracket v)
_DeclBracket' = X._DeclBracket'

_TypeBracket' :: Prism' (Bracket v) (C_TypeBracket v)
_TypeBracket' = X._TypeBracket'

_PatBracket' :: Prism' (Bracket v) (C_PatBracket v)
_PatBracket' = X._PatBracket'

_ExpBracket' :: Prism' (Bracket v) (C_ExpBracket v)
_ExpBracket' = X._ExpBracket'

_RoleWildcard' :: Prism' (Role v) (C_RoleWildcard v)
_RoleWildcard' = X._RoleWildcard'

_Phantom' :: Prism' (Role v) (C_Phantom v)
_Phantom' = X._Phantom'

_Representational' :: Prism' (Role v) (C_Representational v)
_Representational' = X._Representational'

_Nominal' :: Prism' (Role v) (C_Nominal v)
_Nominal' = X._Nominal'

_XDomName' :: Prism' (XName v) (C_XDomName v)
_XDomName' = X._XDomName'

_XName' :: Prism' (XName v) (C_XName v)
_XName' = X._XName'

_IPLin' :: Prism' (IPName v) (C_IPLin v)
_IPLin' = X._IPLin'

_IPDup' :: Prism' (IPName v) (C_IPDup v)
_IPDup' = X._IPDup'

_ConName' :: Prism' (CName v) (C_ConName v)
_ConName' = X._ConName'

_VarName' :: Prism' (CName v) (C_VarName v)
_VarName' = X._VarName'

_UnboxedSingleCon' :: Prism' (SpecialCon v) (C_UnboxedSingleCon v)
_UnboxedSingleCon' = X._UnboxedSingleCon'

_Cons' :: Prism' (SpecialCon v) (C_Cons v)
_Cons' = X._Cons'

_TupleCon' :: Prism' (SpecialCon v) (C_TupleCon v)
_TupleCon' = X._TupleCon'

_FunCon' :: Prism' (SpecialCon v) (C_FunCon v)
_FunCon' = X._FunCon'

_ListCon' :: Prism' (SpecialCon v) (C_ListCon v)
_ListCon' = X._ListCon'

_UnitCon' :: Prism' (SpecialCon v) (C_UnitCon v)
_UnitCon' = X._UnitCon'

_ConOp' :: Prism' (Op v) (C_ConOp v)
_ConOp' = X._ConOp'

_VarOp' :: Prism' (Op v) (C_VarOp v)
_VarOp' = X._VarOp'

_QConOp' :: Prism' (QOp v) (C_QConOp v)
_QConOp' = X._QConOp'

_QVarOp' :: Prism' (QOp v) (C_QVarOp v)
_QVarOp' = X._QVarOp'

_Symbol' :: Prism' (Name v) (C_Symbol v)
_Symbol' = X._Symbol'

_Ident' :: Prism' (Name v) (C_Ident v)
_Ident' = X._Ident'

_Special' :: Prism' (QName v) (C_Special v)
_Special' = X._Special'

_UnQual' :: Prism' (QName v) (C_UnQual v)
_UnQual' = X._UnQual'

_Qual' :: Prism' (QName v) (C_Qual v)
_Qual' = X._Qual'

_Negative' :: Prism' (Sign v) (C_Negative v)
_Negative' = X._Negative'

_Signless' :: Prism' (Sign v) (C_Signless v)
_Signless' = X._Signless'

_PrimString' :: Prism' (Literal v) (C_PrimString v)
_PrimString' = X._PrimString'

_PrimChar' :: Prism' (Literal v) (C_PrimChar v)
_PrimChar' = X._PrimChar'

_PrimDouble' :: Prism' (Literal v) (C_PrimDouble v)
_PrimDouble' = X._PrimDouble'

_PrimFloat' :: Prism' (Literal v) (C_PrimFloat v)
_PrimFloat' = X._PrimFloat'

_PrimWord' :: Prism' (Literal v) (C_PrimWord v)
_PrimWord' = X._PrimWord'

_PrimInt' :: Prism' (Literal v) (C_PrimInt v)
_PrimInt' = X._PrimInt'

_Frac' :: Prism' (Literal v) (C_Frac v)
_Frac' = X._Frac'

_Int' :: Prism' (Literal v) (C_Int v)
_Int' = X._Int'

_String' :: Prism' (Literal v) (C_String v)
_String' = X._String'

_Char' :: Prism' (Literal v) (C_Char v)
_Char' = X._Char'

_RPOptG' :: Prism' (RPatOp v) (C_RPOptG v)
_RPOptG' = X._RPOptG'

_RPOpt' :: Prism' (RPatOp v) (C_RPOpt v)
_RPOpt' = X._RPOpt'

_RPPlusG' :: Prism' (RPatOp v) (C_RPPlusG v)
_RPPlusG' = X._RPPlusG'

_RPPlus' :: Prism' (RPatOp v) (C_RPPlus v)
_RPPlus' = X._RPPlus'

_RPStarG' :: Prism' (RPatOp v) (C_RPStarG v)
_RPStarG' = X._RPStarG'

_RPStar' :: Prism' (RPatOp v) (C_RPStar v)
_RPStar' = X._RPStar'

_RPPat' :: Prism' (RPat v) (C_RPPat v)
_RPPat' = X._RPPat'

_RPParen' :: Prism' (RPat v) (C_RPParen v)
_RPParen' = X._RPParen'

_RPAs' :: Prism' (RPat v) (C_RPAs v)
_RPAs' = X._RPAs'

_RPCAs' :: Prism' (RPat v) (C_RPCAs v)
_RPCAs' = X._RPCAs'

_RPGuard' :: Prism' (RPat v) (C_RPGuard v)
_RPGuard' = X._RPGuard'

_RPSeq' :: Prism' (RPat v) (C_RPSeq v)
_RPSeq' = X._RPSeq'

_RPEither' :: Prism' (RPat v) (C_RPEither v)
_RPEither' = X._RPEither'

_RPOp' :: Prism' (RPat v) (C_RPOp v)
_RPOp' = X._RPOp'

_PFieldWildcard' :: Prism' (PatField v) (C_PFieldWildcard v)
_PFieldWildcard' = X._PFieldWildcard'

_PFieldPun' :: Prism' (PatField v) (C_PFieldPun v)
_PFieldPun' = X._PFieldPun'

_PFieldPat' :: Prism' (PatField v) (C_PFieldPat v)
_PFieldPat' = X._PFieldPat'

_PBangPat' :: Prism' (Pat v) (C_PBangPat v)
_PBangPat' = X._PBangPat'

_PQuasiQuote' :: Prism' (Pat v) (C_PQuasiQuote v)
_PQuasiQuote' = X._PQuasiQuote'

_PXRPats' :: Prism' (Pat v) (C_PXRPats v)
_PXRPats' = X._PXRPats'

_PXPatTag' :: Prism' (Pat v) (C_PXPatTag v)
_PXPatTag' = X._PXPatTag'

_PXPcdata' :: Prism' (Pat v) (C_PXPcdata v)
_PXPcdata' = X._PXPcdata'

_PXETag' :: Prism' (Pat v) (C_PXETag v)
_PXETag' = X._PXETag'

_PXTag' :: Prism' (Pat v) (C_PXTag v)
_PXTag' = X._PXTag'

_PRPat' :: Prism' (Pat v) (C_PRPat v)
_PRPat' = X._PRPat'

_PViewPat' :: Prism' (Pat v) (C_PViewPat v)
_PViewPat' = X._PViewPat'

_PatTypeSig' :: Prism' (Pat v) (C_PatTypeSig v)
_PatTypeSig' = X._PatTypeSig'

_PIrrPat' :: Prism' (Pat v) (C_PIrrPat v)
_PIrrPat' = X._PIrrPat'

_PWildCard' :: Prism' (Pat v) (C_PWildCard v)
_PWildCard' = X._PWildCard'

_PAsPat' :: Prism' (Pat v) (C_PAsPat v)
_PAsPat' = X._PAsPat'

_PRec' :: Prism' (Pat v) (C_PRec v)
_PRec' = X._PRec'

_PParen' :: Prism' (Pat v) (C_PParen v)
_PParen' = X._PParen'

_PList' :: Prism' (Pat v) (C_PList v)
_PList' = X._PList'

_PTuple' :: Prism' (Pat v) (C_PTuple v)
_PTuple' = X._PTuple'

_PApp' :: Prism' (Pat v) (C_PApp v)
_PApp' = X._PApp'

_PInfixApp' :: Prism' (Pat v) (C_PInfixApp v)
_PInfixApp' = X._PInfixApp'

_PNPlusK' :: Prism' (Pat v) (C_PNPlusK v)
_PNPlusK' = X._PNPlusK'

_PLit' :: Prism' (Pat v) (C_PLit v)
_PLit' = X._PLit'

_PVar' :: Prism' (Pat v) (C_PVar v)
_PVar' = X._PVar'

_FieldWildcard' :: Prism' (FieldUpdate v) (C_FieldWildcard v)
_FieldWildcard' = X._FieldWildcard'

_FieldPun' :: Prism' (FieldUpdate v) (C_FieldPun v)
_FieldPun' = X._FieldPun'

_FieldUpdate' :: Prism' (FieldUpdate v) (C_FieldUpdate v)
_FieldUpdate' = X._FieldUpdate'

_GroupByUsing' :: Prism' (QualStmt v) (C_GroupByUsing v)
_GroupByUsing' = X._GroupByUsing'

_GroupUsing' :: Prism' (QualStmt v) (C_GroupUsing v)
_GroupUsing' = X._GroupUsing'

_GroupBy' :: Prism' (QualStmt v) (C_GroupBy v)
_GroupBy' = X._GroupBy'

_ThenBy' :: Prism' (QualStmt v) (C_ThenBy v)
_ThenBy' = X._ThenBy'

_ThenTrans' :: Prism' (QualStmt v) (C_ThenTrans v)
_ThenTrans' = X._ThenTrans'

_QualStmt' :: Prism' (QualStmt v) (C_QualStmt v)
_QualStmt' = X._QualStmt'

_RecStmt' :: Prism' (Stmt v) (C_RecStmt v)
_RecStmt' = X._RecStmt'

_LetStmt' :: Prism' (Stmt v) (C_LetStmt v)
_LetStmt' = X._LetStmt'

_Qualifier' :: Prism' (Stmt v) (C_Qualifier v)
_Qualifier' = X._Qualifier'

_Generator' :: Prism' (Stmt v) (C_Generator v)
_Generator' = X._Generator'

_ExprHole' :: Prism' (Exp v) (C_ExprHole v)
_ExprHole' = X._ExprHole'

_LCase' :: Prism' (Exp v) (C_LCase v)
_LCase' = X._LCase'

_RightArrHighApp' :: Prism' (Exp v) (C_RightArrHighApp v)
_RightArrHighApp' = X._RightArrHighApp'

_LeftArrHighApp' :: Prism' (Exp v) (C_LeftArrHighApp v)
_LeftArrHighApp' = X._LeftArrHighApp'

_RightArrApp' :: Prism' (Exp v) (C_RightArrApp v)
_RightArrApp' = X._RightArrApp'

_LeftArrApp' :: Prism' (Exp v) (C_LeftArrApp v)
_LeftArrApp' = X._LeftArrApp'

_Proc' :: Prism' (Exp v) (C_Proc v)
_Proc' = X._Proc'

_GenPragma' :: Prism' (Exp v) (C_GenPragma v)
_GenPragma' = X._GenPragma'

_SCCPragma' :: Prism' (Exp v) (C_SCCPragma v)
_SCCPragma' = X._SCCPragma'

_CorePragma' :: Prism' (Exp v) (C_CorePragma v)
_CorePragma' = X._CorePragma'

_XChildTag' :: Prism' (Exp v) (C_XChildTag v)
_XChildTag' = X._XChildTag'

_XExpTag' :: Prism' (Exp v) (C_XExpTag v)
_XExpTag' = X._XExpTag'

_XPcdata' :: Prism' (Exp v) (C_XPcdata v)
_XPcdata' = X._XPcdata'

_XETag' :: Prism' (Exp v) (C_XETag v)
_XETag' = X._XETag'

_XTag' :: Prism' (Exp v) (C_XTag v)
_XTag' = X._XTag'

_TypeApp' :: Prism' (Exp v) (C_TypeApp v)
_TypeApp' = X._TypeApp'

_QuasiQuote' :: Prism' (Exp v) (C_QuasiQuote v)
_QuasiQuote' = X._QuasiQuote'

_SpliceExp' :: Prism' (Exp v) (C_SpliceExp v)
_SpliceExp' = X._SpliceExp'

_BracketExp' :: Prism' (Exp v) (C_BracketExp v)
_BracketExp' = X._BracketExp'

_TypQuote' :: Prism' (Exp v) (C_TypQuote v)
_TypQuote' = X._TypQuote'

_VarQuote' :: Prism' (Exp v) (C_VarQuote v)
_VarQuote' = X._VarQuote'

_ExpTypeSig' :: Prism' (Exp v) (C_ExpTypeSig v)
_ExpTypeSig' = X._ExpTypeSig'

_ParArrayComp' :: Prism' (Exp v) (C_ParArrayComp v)
_ParArrayComp' = X._ParArrayComp'

_ParComp' :: Prism' (Exp v) (C_ParComp v)
_ParComp' = X._ParComp'

_ListComp' :: Prism' (Exp v) (C_ListComp v)
_ListComp' = X._ListComp'

_ParArrayFromThenTo' :: Prism' (Exp v) (C_ParArrayFromThenTo v)
_ParArrayFromThenTo' = X._ParArrayFromThenTo'

_ParArrayFromTo' :: Prism' (Exp v) (C_ParArrayFromTo v)
_ParArrayFromTo' = X._ParArrayFromTo'

_EnumFromThenTo' :: Prism' (Exp v) (C_EnumFromThenTo v)
_EnumFromThenTo' = X._EnumFromThenTo'

_EnumFromThen' :: Prism' (Exp v) (C_EnumFromThen v)
_EnumFromThen' = X._EnumFromThen'

_EnumFromTo' :: Prism' (Exp v) (C_EnumFromTo v)
_EnumFromTo' = X._EnumFromTo'

_EnumFrom' :: Prism' (Exp v) (C_EnumFrom v)
_EnumFrom' = X._EnumFrom'

_RecUpdate' :: Prism' (Exp v) (C_RecUpdate v)
_RecUpdate' = X._RecUpdate'

_RecConstr' :: Prism' (Exp v) (C_RecConstr v)
_RecConstr' = X._RecConstr'

_RightSection' :: Prism' (Exp v) (C_RightSection v)
_RightSection' = X._RightSection'

_LeftSection' :: Prism' (Exp v) (C_LeftSection v)
_LeftSection' = X._LeftSection'

_Paren' :: Prism' (Exp v) (C_Paren v)
_Paren' = X._Paren'

_ParArray' :: Prism' (Exp v) (C_ParArray v)
_ParArray' = X._ParArray'

_List' :: Prism' (Exp v) (C_List v)
_List' = X._List'

_TupleSection' :: Prism' (Exp v) (C_TupleSection v)
_TupleSection' = X._TupleSection'

_Tuple' :: Prism' (Exp v) (C_Tuple v)
_Tuple' = X._Tuple'

_MDo' :: Prism' (Exp v) (C_MDo v)
_MDo' = X._MDo'

_Do' :: Prism' (Exp v) (C_Do v)
_Do' = X._Do'

_Case' :: Prism' (Exp v) (C_Case v)
_Case' = X._Case'

_MultiIf' :: Prism' (Exp v) (C_MultiIf v)
_MultiIf' = X._MultiIf'

_If' :: Prism' (Exp v) (C_If v)
_If' = X._If'

_Let' :: Prism' (Exp v) (C_Let v)
_Let' = X._Let'

_Lambda' :: Prism' (Exp v) (C_Lambda v)
_Lambda' = X._Lambda'

_NegApp' :: Prism' (Exp v) (C_NegApp v)
_NegApp' = X._NegApp'

_App' :: Prism' (Exp v) (C_App v)
_App' = X._App'

_InfixApp' :: Prism' (Exp v) (C_InfixApp v)
_InfixApp' = X._InfixApp'

_Lit' :: Prism' (Exp v) (C_Lit v)
_Lit' = X._Lit'

_Con' :: Prism' (Exp v) (C_Con v)
_Con' = X._Con'

_IPVar' :: Prism' (Exp v) (C_IPVar v)
_IPVar' = X._IPVar'

_OverloadedLabel' :: Prism' (Exp v) (C_OverloadedLabel v)
_OverloadedLabel' = X._OverloadedLabel'

_Var' :: Prism' (Exp v) (C_Var v)
_Var' = X._Var'

_PromotedUnit' :: Prism' (Promoted v) (C_PromotedUnit v)
_PromotedUnit' = X._PromotedUnit'

_PromotedTuple' :: Prism' (Promoted v) (C_PromotedTuple v)
_PromotedTuple' = X._PromotedTuple'

_PromotedList' :: Prism' (Promoted v) (C_PromotedList v)
_PromotedList' = X._PromotedList'

_PromotedCon' :: Prism' (Promoted v) (C_PromotedCon v)
_PromotedCon' = X._PromotedCon'

_PromotedString' :: Prism' (Promoted v) (C_PromotedString v)
_PromotedString' = X._PromotedString'

_PromotedInteger' :: Prism' (Promoted v) (C_PromotedInteger v)
_PromotedInteger' = X._PromotedInteger'

_UnkindedVar' :: Prism' (TyVarBind v) (C_UnkindedVar v)
_UnkindedVar' = X._UnkindedVar'

_KindedVar' :: Prism' (TyVarBind v) (C_KindedVar v)
_KindedVar' = X._KindedVar'

_KindList' :: Prism' (Kind v) (C_KindList v)
_KindList' = X._KindList'

_KindTuple' :: Prism' (Kind v) (C_KindTuple v)
_KindTuple' = X._KindTuple'

_KindApp' :: Prism' (Kind v) (C_KindApp v)
_KindApp' = X._KindApp'

_KindVar' :: Prism' (Kind v) (C_KindVar v)
_KindVar' = X._KindVar'

_KindParen' :: Prism' (Kind v) (C_KindParen v)
_KindParen' = X._KindParen'

_KindFn' :: Prism' (Kind v) (C_KindFn v)
_KindFn' = X._KindFn'

_KindStar' :: Prism' (Kind v) (C_KindStar v)
_KindStar' = X._KindStar'

_Unboxed' :: Prism' Boxed C_Unboxed
_Unboxed' = X._Unboxed'

_Boxed' :: Prism' Boxed C_Boxed
_Boxed' = X._Boxed'

_TyQuasiQuote' :: Prism' (Type v) (C_TyQuasiQuote v)
_TyQuasiQuote' = X._TyQuasiQuote'

_TyWildCard' :: Prism' (Type v) (C_TyWildCard v)
_TyWildCard' = X._TyWildCard'

_TyBang' :: Prism' (Type v) (C_TyBang v)
_TyBang' = X._TyBang'

_TySplice' :: Prism' (Type v) (C_TySplice v)
_TySplice' = X._TySplice'

_TyEquals' :: Prism' (Type v) (C_TyEquals v)
_TyEquals' = X._TyEquals'

_TyPromoted' :: Prism' (Type v) (C_TyPromoted v)
_TyPromoted' = X._TyPromoted'

_TyKind' :: Prism' (Type v) (C_TyKind v)
_TyKind' = X._TyKind'

_TyInfix' :: Prism' (Type v) (C_TyInfix v)
_TyInfix' = X._TyInfix'

_TyParen' :: Prism' (Type v) (C_TyParen v)
_TyParen' = X._TyParen'

_TyCon' :: Prism' (Type v) (C_TyCon v)
_TyCon' = X._TyCon'

_TyVar' :: Prism' (Type v) (C_TyVar v)
_TyVar' = X._TyVar'

_TyApp' :: Prism' (Type v) (C_TyApp v)
_TyApp' = X._TyApp'

_TyParArray' :: Prism' (Type v) (C_TyParArray v)
_TyParArray' = X._TyParArray'

_TyList' :: Prism' (Type v) (C_TyList v)
_TyList' = X._TyList'

_TyTuple' :: Prism' (Type v) (C_TyTuple v)
_TyTuple' = X._TyTuple'

_TyFun' :: Prism' (Type v) (C_TyFun v)
_TyFun' = X._TyFun'

_TyForall' :: Prism' (Type v) (C_TyForall v)
_TyForall' = X._TyForall'

_WildCardA' :: Prism' (Asst v) (C_WildCardA v)
_WildCardA' = X._WildCardA'

_ParenA' :: Prism' (Asst v) (C_ParenA v)
_ParenA' = X._ParenA'

_EqualP' :: Prism' (Asst v) (C_EqualP v)
_EqualP' = X._EqualP'

_IParam' :: Prism' (Asst v) (C_IParam v)
_IParam' = X._IParam'

_InfixA' :: Prism' (Asst v) (C_InfixA v)
_InfixA' = X._InfixA'

_AppA' :: Prism' (Asst v) (C_AppA v)
_AppA' = X._AppA'

_ClassA' :: Prism' (Asst v) (C_ClassA v)
_ClassA' = X._ClassA'

_CxEmpty' :: Prism' (Context v) (C_CxEmpty v)
_CxEmpty' = X._CxEmpty'

_CxTuple' :: Prism' (Context v) (C_CxTuple v)
_CxTuple' = X._CxTuple'

_CxSingle' :: Prism' (Context v) (C_CxSingle v)
_CxSingle' = X._CxSingle'

_GuardedRhss' :: Prism' (Rhs v) (C_GuardedRhss v)
_GuardedRhss' = X._GuardedRhss'

_UnGuardedRhs' :: Prism' (Rhs v) (C_UnGuardedRhs v)
_UnGuardedRhs' = X._UnGuardedRhs'

_InfixMatch' :: Prism' (Match v) (C_InfixMatch v)
_InfixMatch' = X._InfixMatch'

_Match' :: Prism' (Match v) (C_Match v)
_Match' = X._Match'

_NoUnpackPragma' :: Prism' (Unpackedness v) (C_NoUnpackPragma v)
_NoUnpackPragma' = X._NoUnpackPragma'

_NoUnpack' :: Prism' (Unpackedness v) (C_NoUnpack v)
_NoUnpack' = X._NoUnpack'

_Unpack' :: Prism' (Unpackedness v) (C_Unpack v)
_Unpack' = X._Unpack'

_NoStrictAnnot' :: Prism' (BangType v) (C_NoStrictAnnot v)
_NoStrictAnnot' = X._NoStrictAnnot'

_LazyTy' :: Prism' (BangType v) (C_LazyTy v)
_LazyTy' = X._LazyTy'

_BangedTy' :: Prism' (BangType v) (C_BangedTy v)
_BangedTy' = X._BangedTy'

_RecDecl' :: Prism' (ConDecl v) (C_RecDecl v)
_RecDecl' = X._RecDecl'

_InfixConDecl' :: Prism' (ConDecl v) (C_InfixConDecl v)
_InfixConDecl' = X._InfixConDecl'

_ConDecl' :: Prism' (ConDecl v) (C_ConDecl v)
_ConDecl' = X._ConDecl'

_NewType' :: Prism' (DataOrNew v) (C_NewType v)
_NewType' = X._NewType'

_DataType' :: Prism' (DataOrNew v) (C_DataType v)
_DataType' = X._DataType'

_InsGData' :: Prism' (InstDecl v) (C_InsGData v)
_InsGData' = X._InsGData'

_InsData' :: Prism' (InstDecl v) (C_InsData v)
_InsData' = X._InsData'

_InsType' :: Prism' (InstDecl v) (C_InsType v)
_InsType' = X._InsType'

_InsDecl' :: Prism' (InstDecl v) (C_InsDecl v)
_InsDecl' = X._InsDecl'

_ClsDefSig' :: Prism' (ClassDecl v) (C_ClsDefSig v)
_ClsDefSig' = X._ClsDefSig'

_ClsTyDef' :: Prism' (ClassDecl v) (C_ClsTyDef v)
_ClsTyDef' = X._ClsTyDef'

_ClsTyFam' :: Prism' (ClassDecl v) (C_ClsTyFam v)
_ClsTyFam' = X._ClsTyFam'

_ClsDataFam' :: Prism' (ClassDecl v) (C_ClsDataFam v)
_ClsDataFam' = X._ClsDataFam'

_ClsDecl' :: Prism' (ClassDecl v) (C_ClsDecl v)
_ClsDecl' = X._ClsDecl'

_TyVarSig' :: Prism' (ResultSig v) (C_TyVarSig v)
_TyVarSig' = X._TyVarSig'

_KindSig' :: Prism' (ResultSig v) (C_KindSig v)
_KindSig' = X._KindSig'

_ExplicitBidirectional' :: Prism' (PatternSynDirection v) (C_ExplicitBidirectional v)
_ExplicitBidirectional' = X._ExplicitBidirectional'

_ImplicitBidirectional' :: Prism' (PatternSynDirection v) (C_ImplicitBidirectional v)
_ImplicitBidirectional' = X._ImplicitBidirectional'

_Unidirectional' :: Prism' (PatternSynDirection v) (C_Unidirectional v)
_Unidirectional' = X._Unidirectional'

_IPBinds' :: Prism' (Binds v) (C_IPBinds v)
_IPBinds' = X._IPBinds'

_BDecls' :: Prism' (Binds v) (C_BDecls v)
_BDecls' = X._BDecls'

_IHApp' :: Prism' (InstHead v) (C_IHApp v)
_IHApp' = X._IHApp'

_IHParen' :: Prism' (InstHead v) (C_IHParen v)
_IHParen' = X._IHParen'

_IHInfix' :: Prism' (InstHead v) (C_IHInfix v)
_IHInfix' = X._IHInfix'

_IHCon' :: Prism' (InstHead v) (C_IHCon v)
_IHCon' = X._IHCon'

_IParen' :: Prism' (InstRule v) (C_IParen v)
_IParen' = X._IParen'

_IRule' :: Prism' (InstRule v) (C_IRule v)
_IRule' = X._IRule'

_DHApp' :: Prism' (DeclHead v) (C_DHApp v)
_DHApp' = X._DHApp'

_DHParen' :: Prism' (DeclHead v) (C_DHParen v)
_DHParen' = X._DHParen'

_DHInfix' :: Prism' (DeclHead v) (C_DHInfix v)
_DHInfix' = X._DHInfix'

_DHead' :: Prism' (DeclHead v) (C_DHead v)
_DHead' = X._DHead'

_RoleAnnotDecl' :: Prism' (Decl v) (C_RoleAnnotDecl v)
_RoleAnnotDecl' = X._RoleAnnotDecl'

_MinimalPragma' :: Prism' (Decl v) (C_MinimalPragma v)
_MinimalPragma' = X._MinimalPragma'

_AnnPragma' :: Prism' (Decl v) (C_AnnPragma v)
_AnnPragma' = X._AnnPragma'

_InstSig' :: Prism' (Decl v) (C_InstSig v)
_InstSig' = X._InstSig'

_SpecInlineSig' :: Prism' (Decl v) (C_SpecInlineSig v)
_SpecInlineSig' = X._SpecInlineSig'

_SpecSig' :: Prism' (Decl v) (C_SpecSig v)
_SpecSig' = X._SpecSig'

_InlineConlikeSig' :: Prism' (Decl v) (C_InlineConlikeSig v)
_InlineConlikeSig' = X._InlineConlikeSig'

_InlineSig' :: Prism' (Decl v) (C_InlineSig v)
_InlineSig' = X._InlineSig'

_WarnPragmaDecl' :: Prism' (Decl v) (C_WarnPragmaDecl v)
_WarnPragmaDecl' = X._WarnPragmaDecl'

_DeprPragmaDecl' :: Prism' (Decl v) (C_DeprPragmaDecl v)
_DeprPragmaDecl' = X._DeprPragmaDecl'

_RulePragmaDecl' :: Prism' (Decl v) (C_RulePragmaDecl v)
_RulePragmaDecl' = X._RulePragmaDecl'

_ForExp' :: Prism' (Decl v) (C_ForExp v)
_ForExp' = X._ForExp'

_ForImp' :: Prism' (Decl v) (C_ForImp v)
_ForImp' = X._ForImp'

_PatSyn' :: Prism' (Decl v) (C_PatSyn v)
_PatSyn' = X._PatSyn'

_PatBind' :: Prism' (Decl v) (C_PatBind v)
_PatBind' = X._PatBind'

_FunBind' :: Prism' (Decl v) (C_FunBind v)
_FunBind' = X._FunBind'

_PatSynSig' :: Prism' (Decl v) (C_PatSynSig v)
_PatSynSig' = X._PatSynSig'

_TypeSig' :: Prism' (Decl v) (C_TypeSig v)
_TypeSig' = X._TypeSig'

_SpliceDecl' :: Prism' (Decl v) (C_SpliceDecl v)
_SpliceDecl' = X._SpliceDecl'

_DefaultDecl' :: Prism' (Decl v) (C_DefaultDecl v)
_DefaultDecl' = X._DefaultDecl'

_InfixDecl' :: Prism' (Decl v) (C_InfixDecl v)
_InfixDecl' = X._InfixDecl'

_DerivDecl' :: Prism' (Decl v) (C_DerivDecl v)
_DerivDecl' = X._DerivDecl'

_InstDecl' :: Prism' (Decl v) (C_InstDecl v)
_InstDecl' = X._InstDecl'

_ClassDecl' :: Prism' (Decl v) (C_ClassDecl v)
_ClassDecl' = X._ClassDecl'

_GDataInsDecl' :: Prism' (Decl v) (C_GDataInsDecl v)
_GDataInsDecl' = X._GDataInsDecl'

_DataInsDecl' :: Prism' (Decl v) (C_DataInsDecl v)
_DataInsDecl' = X._DataInsDecl'

_TypeInsDecl' :: Prism' (Decl v) (C_TypeInsDecl v)
_TypeInsDecl' = X._TypeInsDecl'

_DataFamDecl' :: Prism' (Decl v) (C_DataFamDecl v)
_DataFamDecl' = X._DataFamDecl'

_GDataDecl' :: Prism' (Decl v) (C_GDataDecl v)
_GDataDecl' = X._GDataDecl'

_DataDecl' :: Prism' (Decl v) (C_DataDecl v)
_DataDecl' = X._DataDecl'

_ClosedTypeFamDecl' :: Prism' (Decl v) (C_ClosedTypeFamDecl v)
_ClosedTypeFamDecl' = X._ClosedTypeFamDecl'

_TypeFamDecl' :: Prism' (Decl v) (C_TypeFamDecl v)
_TypeFamDecl' = X._TypeFamDecl'

_TypeDecl' :: Prism' (Decl v) (C_TypeDecl v)
_TypeDecl' = X._TypeDecl'

_PatternNamespace' :: Prism' (Namespace v) (C_PatternNamespace v)
_PatternNamespace' = X._PatternNamespace'

_TypeNamespace' :: Prism' (Namespace v) (C_TypeNamespace v)
_TypeNamespace' = X._TypeNamespace'

_NoNamespace' :: Prism' (Namespace v) (C_NoNamespace v)
_NoNamespace' = X._NoNamespace'

_AssocRight' :: Prism' (Assoc v) (C_AssocRight v)
_AssocRight' = X._AssocRight'

_AssocLeft' :: Prism' (Assoc v) (C_AssocLeft v)
_AssocLeft' = X._AssocLeft'

_AssocNone' :: Prism' (Assoc v) (C_AssocNone v)
_AssocNone' = X._AssocNone'

_IThingWith' :: Prism' (ImportSpec v) (C_IThingWith v)
_IThingWith' = X._IThingWith'

_IThingAll' :: Prism' (ImportSpec v) (C_IThingAll v)
_IThingAll' = X._IThingAll'

_IAbs' :: Prism' (ImportSpec v) (C_IAbs v)
_IAbs' = X._IAbs'

_IVar' :: Prism' (ImportSpec v) (C_IVar v)
_IVar' = X._IVar'

_EWildcard' :: Prism' (EWildcard v) (C_EWildcard v)
_EWildcard' = X._EWildcard'

_NoWildcard' :: Prism' (EWildcard v) (C_NoWildcard v)
_NoWildcard' = X._NoWildcard'

_EModuleContents' :: Prism' (ExportSpec v) (C_EModuleContents v)
_EModuleContents' = X._EModuleContents'

_EThingWith' :: Prism' (ExportSpec v) (C_EThingWith v)
_EThingWith' = X._EThingWith'

_EAbs' :: Prism' (ExportSpec v) (C_EAbs v)
_EAbs' = X._EAbs'

_EVar' :: Prism' (ExportSpec v) (C_EVar v)
_EVar' = X._EVar'

_WarnText' :: Prism' (WarningText v) (C_WarnText v)
_WarnText' = X._WarnText'

_DeprText' :: Prism' (WarningText v) (C_DeprText v)
_DeprText' = X._DeprText'

_XmlHybrid' :: Prism' (Module v) (C_XmlHybrid v)
_XmlHybrid' = X._XmlHybrid'

_XmlPage' :: Prism' (Module v) (C_XmlPage v)
_XmlPage' = X._XmlPage'

_Module' :: Prism' (Module v) (C_Module v)
_Module' = X._Module'

_Rule :: Iso'
         ( Rule SrcSpanInfo )
         ( SrcSpanInfo, String, Maybe (Activation SrcSpanInfo)
         , Maybe [RuleVar SrcSpanInfo], Exp SrcSpanInfo, Exp SrcSpanInfo
         )
_Rule = X._Rule

_Incoherent :: Prism' (Overlap SrcSpanInfo) SrcSpanInfo
_Incoherent = X._Incoherent

_ModuleName :: Iso' (ModuleName SrcSpanInfo) (SrcSpanInfo, String)
_ModuleName = X._ModuleName

_PXAttr :: Iso'
           (PXAttr SrcSpanInfo)
           (SrcSpanInfo, XName SrcSpanInfo, Pat SrcSpanInfo)
_PXAttr = X._PXAttr

_XAttr :: Iso'
          (XAttr SrcSpanInfo)
          (SrcSpanInfo, XName SrcSpanInfo, Exp SrcSpanInfo)
_XAttr = X._XAttr

_Alt :: Iso'
        (Alt SrcSpanInfo)
        (SrcSpanInfo, Pat SrcSpanInfo, Rhs SrcSpanInfo, Maybe (Binds SrcSpanInfo))
_Alt = X._Alt

_TypeEqn :: Iso'
            (TypeEqn SrcSpanInfo)
            (SrcSpanInfo, Type SrcSpanInfo, Type SrcSpanInfo)
_TypeEqn = X._TypeEqn

_FunDep :: Iso' (FunDep SrcSpanInfo) (SrcSpanInfo, [Name SrcSpanInfo], [Name SrcSpanInfo])
_FunDep = X._FunDep

_GuardedRhs :: Iso' (GuardedRhs SrcSpanInfo) (SrcSpanInfo, [Stmt SrcSpanInfo], Exp SrcSpanInfo)
_GuardedRhs = X._GuardedRhs

_GadtDecl :: Iso' (GadtDecl SrcSpanInfo) (SrcSpanInfo, Name SrcSpanInfo, Maybe [FieldDecl SrcSpanInfo], Type SrcSpanInfo)
_GadtDecl = X._GadtDecl

_QualConDecl :: Iso'
                (QualConDecl SrcSpanInfo)
                (SrcSpanInfo, Maybe [TyVarBind SrcSpanInfo], Maybe (Context SrcSpanInfo), ConDecl SrcSpanInfo)
_QualConDecl = X._QualConDecl

_FieldDecl :: Iso' (FieldDecl SrcSpanInfo) (SrcSpanInfo, [Name SrcSpanInfo], Type SrcSpanInfo)
_FieldDecl = X._FieldDecl

_Deriving :: Iso' (Deriving SrcSpanInfo) (SrcSpanInfo, [InstRule SrcSpanInfo])
_Deriving = X._Deriving

_InjectivityInfo :: Iso' (InjectivityInfo SrcSpanInfo) (SrcSpanInfo, Name SrcSpanInfo, [Name SrcSpanInfo])
_InjectivityInfo = X._InjectivityInfo

_ExplicitBidirectional :: Prism' (PatternSynDirection SrcSpanInfo) (SrcSpanInfo, [Decl SrcSpanInfo])
_ExplicitBidirectional = X._ExplicitBidirectional

_IPBind :: Iso' (IPBind SrcSpanInfo) (SrcSpanInfo, IPName SrcSpanInfo, Exp SrcSpanInfo)
_IPBind = X._IPBind

_ImportSpecList :: Iso' (ImportSpecList SrcSpanInfo) (SrcSpanInfo, Bool, [ImportSpec SrcSpanInfo])
_ImportSpecList = X._ImportSpecList

_ImportDecl :: Iso'
               ( ImportDecl SrcSpanInfo )
               ( SrcSpanInfo, ModuleName SrcSpanInfo, Bool, Bool, Bool, Maybe String, Maybe (ModuleName SrcSpanInfo)
               , Maybe (ImportSpecList SrcSpanInfo)
               )
_ImportDecl = X._ImportDecl

_ExportSpecList :: Iso' (ExportSpecList SrcSpanInfo) (SrcSpanInfo, [ExportSpec SrcSpanInfo])
_ExportSpecList = X._ExportSpecList

_ModuleHead :: Iso'
               (ModuleHead SrcSpanInfo)
               (SrcSpanInfo, ModuleName SrcSpanInfo, Maybe (WarningText SrcSpanInfo), Maybe (ExportSpecList SrcSpanInfo))
_ModuleHead = X._ModuleHead
