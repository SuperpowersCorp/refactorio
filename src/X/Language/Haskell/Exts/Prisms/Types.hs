{-# LANGUAGE NoImplicitPrelude #-}

module X.Language.Haskell.Exts.Prisms.Types ( module Exports ) where

import Language.Haskell.Exts.Prisms as Exports ( C_ActiveFrom(..)
                                               , C_ActiveUntil(..)
                                               , C_AndFormula(..)
                                               , C_Ann(..)
                                               , C_AnnModulePragma(..)
                                               , C_AnnPragma(..)
                                               , C_App(..)
                                               , C_AppA(..)
                                               , C_AssocLeft(..)
                                               , C_AssocNone(..)
                                               , C_AssocRight(..)
                                               , C_BDecls(..)
                                               , C_BangedTy(..)
                                               , C_Boxed(..)
                                               , C_BracketExp(..)
                                               , C_CApi(..)
                                               , C_CCall(..)
                                               , C_CPlusPlus(..)
                                               , C_Case(..)
                                               , C_Char(..)
                                               , C_ClassA(..)
                                               , C_ClassDecl(..)
                                               , C_ClosedTypeFamDecl(..)
                                               , C_ClsDataFam(..)
                                               , C_ClsDecl(..)
                                               , C_ClsDefSig(..)
                                               , C_ClsTyDef(..)
                                               , C_ClsTyFam(..)
                                               , C_Con(..)
                                               , C_ConDecl(..)
                                               , C_ConName(..)
                                               , C_ConOp(..)
                                               , C_Cons(..)
                                               , C_CorePragma(..)
                                               , C_CxEmpty(..)
                                               , C_CxSingle(..)
                                               , C_CxTuple(..)
                                               , C_DHApp(..)
                                               , C_DHInfix(..)
                                               , C_DHParen(..)
                                               , C_DHead(..)
                                               , C_DataDecl(..)
                                               , C_DataFamDecl(..)
                                               , C_DataInsDecl(..)
                                               , C_DataType(..)
                                               , C_DeclBracket(..)
                                               , C_DefaultDecl(..)
                                               , C_DeprPragmaDecl(..)
                                               , C_DeprText(..)
                                               , C_DeprText(..)
                                               , C_DerivDecl(..)
                                               , C_Do(..)
                                               , C_DotNet(..)
                                               , C_EAbs(..)
                                               , C_EModuleContents(..)
                                               , C_EThingWith(..)
                                               , C_EVar(..)
                                               , C_EWildcard(..)
                                               , C_EnumFrom(..)
                                               , C_EnumFromThen(..)
                                               , C_EnumFromThenTo(..)
                                               , C_EnumFromTo(..)
                                               , C_EqualP(..)
                                               , C_ExpBracket(..)
                                               , C_ExpTypeSig(..)
                                               , C_ExplicitBidirectional(..)
                                               , C_ExprHole(..)
                                               , C_FieldPun(..)
                                               , C_FieldUpdate(..)
                                               , C_FieldWildcard(..)
                                               , C_ForExp(..)
                                               , C_ForImp(..)
                                               , C_Frac(..)
                                               , C_FunBind(..)
                                               , C_FunCon(..)
                                               , C_GDataDecl(..)
                                               , C_GDataInsDecl(..)
                                               , C_GHC(..)
                                               , C_GenPragma(..)
                                               , C_Generator(..)
                                               , C_GroupBy(..)
                                               , C_GroupByUsing(..)
                                               , C_GroupUsing(..)
                                               , C_GuardedRhss(..)
                                               , C_HADDOCK(..)
                                               , C_HUGS(..)
                                               , C_IAbs(..)
                                               , C_IHApp(..)
                                               , C_IHCon(..)
                                               , C_IHInfix(..)
                                               , C_IHParen(..)
                                               , C_IPBinds(..)
                                               , C_IPDup(..)
                                               , C_IPLin(..)
                                               , C_IPVar(..)
                                               , C_IParam(..)
                                               , C_IParen(..)
                                               , C_IRule(..)
                                               , C_IThingAll(..)
                                               , C_IThingWith(..)
                                               , C_IVar(..)
                                               , C_IdSplice(..)
                                               , C_Ident(..)
                                               , C_If(..)
                                               , C_ImplicitBidirectional(..)
                                               , C_Incoherent(..)
                                               , C_InfixA(..)
                                               , C_InfixApp(..)
                                               , C_InfixConDecl(..)
                                               , C_InfixDecl(..)
                                               , C_InfixMatch(..)
                                               , C_InlineConlikeSig(..)
                                               , C_InlineSig(..)
                                               , C_InsData(..)
                                               , C_InsDecl(..)
                                               , C_InsGData(..)
                                               , C_InsType(..)
                                               , C_InstDecl(..)
                                               , C_InstSig(..)
                                               , C_Int(..)
                                               , C_JavaScript(..)
                                               , C_Js(..)
                                               , C_Jvm(..)
                                               , C_KindApp(..)
                                               , C_KindFn(..)
                                               , C_KindList(..)
                                               , C_KindParen(..)
                                               , C_KindSig(..)
                                               , C_KindStar(..)
                                               , C_KindTuple(..)
                                               , C_KindVar(..)
                                               , C_KindedVar(..)
                                               , C_LCase(..)
                                               , C_Lambda(..)
                                               , C_LanguagePragma(..)
                                               , C_LazyTy(..)
                                               , C_LeftArrApp(..)
                                               , C_LeftArrHighApp(..)
                                               , C_LeftSection(..)
                                               , C_Let(..)
                                               , C_LetStmt(..)
                                               , C_List(..)
                                               , C_ListComp(..)
                                               , C_ListCon(..)
                                               , C_Lit(..)
                                               , C_MDo(..)
                                               , C_Match(..)
                                               , C_MinimalPragma(..)
                                               , C_Module(..)
                                               , C_ModuleAnn(..)
                                               , C_MultiIf(..)
                                               , C_NHC98(..)
                                               , C_NegApp(..)
                                               , C_Negative(..)
                                               , C_NewType(..)
                                               , C_NoNamespace(..)
                                               , C_NoOverlap(..)
                                               , C_NoStrictAnnot(..)
                                               , C_NoUnpack(..)
                                               , C_NoUnpackPragma(..)
                                               , C_NoWildcard(..)
                                               , C_Nominal(..)
                                               , C_OptionsPragma(..)
                                               , C_OrFormula(..)
                                               , C_Overlap(..)
                                               , C_OverloadedLabel(..)
                                               , C_PApp(..)
                                               , C_PAsPat(..)
                                               , C_PBangPat(..)
                                               , C_PFieldPat(..)
                                               , C_PFieldPun(..)
                                               , C_PFieldWildcard(..)
                                               , C_PInfixApp(..)
                                               , C_PIrrPat(..)
                                               , C_PList(..)
                                               , C_PLit(..)
                                               , C_PNPlusK(..)
                                               , C_PParen(..)
                                               , C_PQuasiQuote(..)
                                               , C_PRPat(..)
                                               , C_PRec(..)
                                               , C_PTuple(..)
                                               , C_PVar(..)
                                               , C_PViewPat(..)
                                               , C_PWildCard(..)
                                               , C_PXETag(..)
                                               , C_PXPatTag(..)
                                               , C_PXPcdata(..)
                                               , C_PXRPats(..)
                                               , C_PXTag(..)
                                               , C_ParArray(..)
                                               , C_ParArrayComp(..)
                                               , C_ParArrayFromThenTo(..)
                                               , C_ParArrayFromTo(..)
                                               , C_ParComp(..)
                                               , C_Paren(..)
                                               , C_ParenA(..)
                                               , C_ParenFormula(..)
                                               , C_ParenSplice(..)
                                               , C_PatBind(..)
                                               , C_PatBracket(..)
                                               , C_PatSyn(..)
                                               , C_PatSynSig(..)
                                               , C_PatTypeSig(..)
                                               , C_PatternNamespace(..)
                                               , C_Phantom(..)
                                               , C_PlayInterruptible(..)
                                               , C_PlayRisky(..)
                                               , C_PlaySafe(..)
                                               , C_PrimChar(..)
                                               , C_PrimDouble(..)
                                               , C_PrimFloat(..)
                                               , C_PrimInt(..)
                                               , C_PrimString(..)
                                               , C_PrimWord(..)
                                               , C_Proc(..)
                                               , C_PromotedCon(..)
                                               , C_PromotedInteger(..)
                                               , C_PromotedList(..)
                                               , C_PromotedString(..)
                                               , C_PromotedTuple(..)
                                               , C_PromotedUnit(..)
                                               , C_QConOp(..)
                                               , C_QVarOp(..)
                                               , C_Qual(..)
                                               , C_QualStmt(..)
                                               , C_Qualifier(..)
                                               , C_QuasiQuote(..)
                                               , C_RPAs(..)
                                               , C_RPCAs(..)
                                               , C_RPEither(..)
                                               , C_RPGuard(..)
                                               , C_RPOp(..)
                                               , C_RPOpt(..)
                                               , C_RPOptG(..)
                                               , C_RPParen(..)
                                               , C_RPPat(..)
                                               , C_RPPlus(..)
                                               , C_RPPlusG(..)
                                               , C_RPSeq(..)
                                               , C_RPStar(..)
                                               , C_RPStarG(..)
                                               , C_RecConstr(..)
                                               , C_RecDecl(..)
                                               , C_RecStmt(..)
                                               , C_RecUpdate(..)
                                               , C_Representational(..)
                                               , C_RightArrApp(..)
                                               , C_RightArrHighApp(..)
                                               , C_RightSection(..)
                                               , C_RoleAnnotDecl(..)
                                               , C_RoleWildcard(..)
                                               , C_RulePragmaDecl(..)
                                               , C_RuleVar(..)
                                               , C_SCCPragma(..)
                                               , C_Signless(..)
                                               , C_SpecInlineSig(..)
                                               , C_SpecSig(..)
                                               , C_Special(..)
                                               , C_SpliceDecl(..)
                                               , C_SpliceExp(..)
                                               , C_StdCall(..)
                                               , C_String(..)
                                               , C_Symbol(..)
                                               , C_ThenBy(..)
                                               , C_ThenTrans(..)
                                               , C_Tuple(..)
                                               , C_TupleCon(..)
                                               , C_TupleSection(..)
                                               , C_TyApp(..)
                                               , C_TyBang(..)
                                               , C_TyCon(..)
                                               , C_TyEquals(..)
                                               , C_TyForall(..)
                                               , C_TyFun(..)
                                               , C_TyInfix(..)
                                               , C_TyKind(..)
                                               , C_TyList(..)
                                               , C_TyParArray(..)
                                               , C_TyParen(..)
                                               , C_TyPromoted(..)
                                               , C_TyQuasiQuote(..)
                                               , C_TySplice(..)
                                               , C_TyTuple(..)
                                               , C_TyVar(..)
                                               , C_TyVarSig(..)
                                               , C_TyWildCard(..)
                                               , C_TypQuote(..)
                                               , C_TypeAnn(..)
                                               , C_TypeApp(..)
                                               , C_TypeBracket(..)
                                               , C_TypeDecl(..)
                                               , C_TypeDecl(..)
                                               , C_TypeFamDecl(..)
                                               , C_TypeInsDecl(..)
                                               , C_TypeNamespace(..)
                                               , C_TypeSig(..)
                                               , C_TypedRuleVar(..)
                                               , C_UnGuardedRhs(..)
                                               , C_UnQual(..)
                                               , C_Unboxed(..)
                                               , C_UnboxedSingleCon(..)
                                               , C_Unidirectional(..)
                                               , C_UnitCon(..)
                                               , C_UnkindedVar(..)
                                               , C_UnknownTool(..)
                                               , C_Unpack(..)
                                               , C_Var(..)
                                               , C_VarFormula(..)
                                               , C_VarName(..)
                                               , C_VarOp(..)
                                               , C_VarQuote(..)
                                               , C_WarnPragmaDecl(..)
                                               , C_WarnText(..)
                                               , C_WarnText(..)
                                               , C_WildCardA(..)
                                               , C_XChildTag(..)
                                               , C_XDomName(..)
                                               , C_XETag(..)
                                               , C_XExpTag(..)
                                               , C_XName(..)
                                               , C_XPcdata(..)
                                               , C_XTag(..)
                                               , C_XmlHybrid(..)
                                               , C_XmlPage(..)
                                               , C_YHC(..)
                                               )
