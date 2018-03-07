

-- UUAGC 0.9.52.1 (src/Language/Haskell/AG/Syntax.ag)

{-# LINE 32 "src/Language/Haskell/AG/Syntax.ag" #-}

{-# OPTIONS_GHC -fno-warn-unused-matches -fno-warn-unused-pattern-binds #-}
{-# LANGUAGE DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, StandaloneDeriving, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Language.Haskell.AG.Syntax where

--import Data.Data
--import GHC.Generics
import Language.Haskell.Exts.Syntax
{-# LINE 15 "src/Language/Haskell/AG/Syntax.hs" #-}
-- Activation --------------------------------------------------
-- cata
sem_Activation :: (Activation) (l) ->
                  (T_Activation) (l)
sem_Activation (ActiveFrom _loca _int) =
    (sem_Activation_ActiveFrom _loca _int)
sem_Activation (ActiveUntil _loca _int) =
    (sem_Activation_ActiveUntil _loca _int)
-- semantic domain
type T_Activation l = ( )
data Inh_Activation l = Inh_Activation {}
data Syn_Activation l = Syn_Activation {}
wrap_Activation :: (T_Activation) (l) ->
                   (Inh_Activation) (l) ->
                   (Syn_Activation) (l)
wrap_Activation sem (Inh_Activation) =
    (let ( ) = sem
     in  (Syn_Activation))
sem_Activation_ActiveFrom :: (l) ->
                             (Int) ->
                             (T_Activation) (l)
sem_Activation_ActiveFrom (loca_ :: (l)) (int_ :: (Int)) =
    (let
     in  ( ))
sem_Activation_ActiveUntil :: (l) ->
                              (Int) ->
                              (T_Activation) (l)
sem_Activation_ActiveUntil (loca_ :: (l)) (int_ :: (Int)) =
    (let
     in  ( ))
-- Alt ---------------------------------------------------------
-- cata
sem_Alt :: (Alt) (l) ->
           (T_Alt) (l)
sem_Alt (Alt _loca _pat _rhs _mBinds) =
    (sem_Alt_Alt _loca _pat _rhs _mBinds)
-- semantic domain
type T_Alt l = ( )
data Inh_Alt l = Inh_Alt {}
data Syn_Alt l = Syn_Alt {}
wrap_Alt :: (T_Alt) (l) ->
            (Inh_Alt) (l) ->
            (Syn_Alt) (l)
wrap_Alt sem (Inh_Alt) =
    (let ( ) = sem
     in  (Syn_Alt))
sem_Alt_Alt :: (l) ->
               (Pat l) ->
               (Rhs l) ->
               (Maybe (Binds l)) ->
               (T_Alt) (l)
sem_Alt_Alt (loca_ :: (l)) (pat_ :: (Pat l)) (rhs_ :: (Rhs l)) (mBinds_ :: (Maybe (Binds l))) =
    (let
     in  ( ))
-- Annotation --------------------------------------------------
-- cata
sem_Annotation :: (Annotation) (l) ->
                  (T_Annotation) (l)
sem_Annotation (Ann _loca _name _exp) =
    (sem_Annotation_Ann _loca _name _exp)
sem_Annotation (TypeAnn _loca _name _exp) =
    (sem_Annotation_TypeAnn _loca _name _exp)
sem_Annotation (ModuleAnn _loca _exp) =
    (sem_Annotation_ModuleAnn _loca _exp)
-- semantic domain
type T_Annotation l = ( )
data Inh_Annotation l = Inh_Annotation {}
data Syn_Annotation l = Syn_Annotation {}
wrap_Annotation :: (T_Annotation) (l) ->
                   (Inh_Annotation) (l) ->
                   (Syn_Annotation) (l)
wrap_Annotation sem (Inh_Annotation) =
    (let ( ) = sem
     in  (Syn_Annotation))
sem_Annotation_Ann :: (l) ->
                      (Name l) ->
                      (Exp l) ->
                      (T_Annotation) (l)
sem_Annotation_Ann (loca_ :: (l)) (name_ :: (Name l)) (exp_ :: (Exp l)) =
    (let
     in  ( ))
sem_Annotation_TypeAnn :: (l) ->
                          (Name l) ->
                          (Exp l) ->
                          (T_Annotation) (l)
sem_Annotation_TypeAnn (loca_ :: (l)) (name_ :: (Name l)) (exp_ :: (Exp l)) =
    (let
     in  ( ))
sem_Annotation_ModuleAnn :: (l) ->
                            (Exp l) ->
                            (T_Annotation) (l)
sem_Annotation_ModuleAnn (loca_ :: (l)) (exp_ :: (Exp l)) =
    (let
     in  ( ))
-- Assoc -------------------------------------------------------
-- cata
sem_Assoc :: (Assoc) (l) ->
             (T_Assoc) (l)
sem_Assoc (AssocNone _loca) =
    (sem_Assoc_AssocNone _loca)
sem_Assoc (AssocLeft _loca) =
    (sem_Assoc_AssocLeft _loca)
sem_Assoc (AssocRight _loca) =
    (sem_Assoc_AssocRight _loca)
-- semantic domain
type T_Assoc l = ( )
data Inh_Assoc l = Inh_Assoc {}
data Syn_Assoc l = Syn_Assoc {}
wrap_Assoc :: (T_Assoc) (l) ->
              (Inh_Assoc) (l) ->
              (Syn_Assoc) (l)
wrap_Assoc sem (Inh_Assoc) =
    (let ( ) = sem
     in  (Syn_Assoc))
sem_Assoc_AssocNone :: (l) ->
                       (T_Assoc) (l)
sem_Assoc_AssocNone (loca_ :: (l)) =
    (let
     in  ( ))
sem_Assoc_AssocLeft :: (l) ->
                       (T_Assoc) (l)
sem_Assoc_AssocLeft (loca_ :: (l)) =
    (let
     in  ( ))
sem_Assoc_AssocRight :: (l) ->
                        (T_Assoc) (l)
sem_Assoc_AssocRight (loca_ :: (l)) =
    (let
     in  ( ))
-- Asst --------------------------------------------------------
-- cata
sem_Asst :: (Asst) (l) ->
            (T_Asst) (l)
sem_Asst (ClassA _loca _qName _types) =
    (sem_Asst_ClassA _loca _qName _types)
sem_Asst (AppA _loca _name _types) =
    (sem_Asst_AppA _loca _name _types)
sem_Asst (InfixA _loca _typ1 _qName _typ2) =
    (sem_Asst_InfixA _loca _typ1 _qName _typ2)
sem_Asst (IParam _loca _iPName _typ) =
    (sem_Asst_IParam _loca _iPName _typ)
sem_Asst (EqualP _loca _typ1 _typ2) =
    (sem_Asst_EqualP _loca _typ1 _typ2)
sem_Asst (ParenA _loca _asst) =
    (sem_Asst_ParenA _loca _asst)
sem_Asst (WildCardA _loca _mName) =
    (sem_Asst_WildCardA _loca _mName)
-- semantic domain
type T_Asst l = ( )
data Inh_Asst l = Inh_Asst {}
data Syn_Asst l = Syn_Asst {}
wrap_Asst :: (T_Asst) (l) ->
             (Inh_Asst) (l) ->
             (Syn_Asst) (l)
wrap_Asst sem (Inh_Asst) =
    (let ( ) = sem
     in  (Syn_Asst))
sem_Asst_ClassA :: (l) ->
                   (QName l) ->
                   ([Type l]) ->
                   (T_Asst) (l)
sem_Asst_ClassA (loca_ :: (l)) (qName_ :: (QName l)) (types_ :: ([Type l])) =
    (let
     in  ( ))
sem_Asst_AppA :: (l) ->
                 (Name l) ->
                 ([Type l]) ->
                 (T_Asst) (l)
sem_Asst_AppA (loca_ :: (l)) (name_ :: (Name l)) (types_ :: ([Type l])) =
    (let
     in  ( ))
sem_Asst_InfixA :: (l) ->
                   (Type l) ->
                   (QName l) ->
                   (Type l) ->
                   (T_Asst) (l)
sem_Asst_InfixA (loca_ :: (l)) (typ1_ :: (Type l)) (qName_ :: (QName l)) (typ2_ :: (Type l)) =
    (let
     in  ( ))
sem_Asst_IParam :: (l) ->
                   (IPName l) ->
                   (Type l) ->
                   (T_Asst) (l)
sem_Asst_IParam (loca_ :: (l)) (iPName_ :: (IPName l)) (typ_ :: (Type l)) =
    (let
     in  ( ))
sem_Asst_EqualP :: (l) ->
                   (Type l) ->
                   (Type l) ->
                   (T_Asst) (l)
sem_Asst_EqualP (loca_ :: (l)) (typ1_ :: (Type l)) (typ2_ :: (Type l)) =
    (let
     in  ( ))
sem_Asst_ParenA :: (l) ->
                   (Asst l) ->
                   (T_Asst) (l)
sem_Asst_ParenA (loca_ :: (l)) (asst_ :: (Asst l)) =
    (let
     in  ( ))
sem_Asst_WildCardA :: (l) ->
                      (Maybe (Name l)) ->
                      (T_Asst) (l)
sem_Asst_WildCardA (loca_ :: (l)) (mName_ :: (Maybe (Name l))) =
    (let
     in  ( ))
-- BangType ----------------------------------------------------
-- cata
sem_BangType :: (BangType) (l) ->
                (T_BangType) (l)
sem_BangType (BangedTy _loca) =
    (sem_BangType_BangedTy _loca)
sem_BangType (LazyTy _loca) =
    (sem_BangType_LazyTy _loca)
sem_BangType (NoStrictAnnot _loca) =
    (sem_BangType_NoStrictAnnot _loca)
-- semantic domain
type T_BangType l = ( )
data Inh_BangType l = Inh_BangType {}
data Syn_BangType l = Syn_BangType {}
wrap_BangType :: (T_BangType) (l) ->
                 (Inh_BangType) (l) ->
                 (Syn_BangType) (l)
wrap_BangType sem (Inh_BangType) =
    (let ( ) = sem
     in  (Syn_BangType))
sem_BangType_BangedTy :: (l) ->
                         (T_BangType) (l)
sem_BangType_BangedTy (loca_ :: (l)) =
    (let
     in  ( ))
sem_BangType_LazyTy :: (l) ->
                       (T_BangType) (l)
sem_BangType_LazyTy (loca_ :: (l)) =
    (let
     in  ( ))
sem_BangType_NoStrictAnnot :: (l) ->
                              (T_BangType) (l)
sem_BangType_NoStrictAnnot (loca_ :: (l)) =
    (let
     in  ( ))
-- Binds -------------------------------------------------------
-- cata
sem_Binds :: (Binds) (l) ->
             (T_Binds) (l)
sem_Binds (BDecls _loca _decls) =
    (sem_Binds_BDecls _loca _decls)
sem_Binds (IPBinds _loca _iPBinds) =
    (sem_Binds_IPBinds _loca _iPBinds)
-- semantic domain
type T_Binds l = ( )
data Inh_Binds l = Inh_Binds {}
data Syn_Binds l = Syn_Binds {}
wrap_Binds :: (T_Binds) (l) ->
              (Inh_Binds) (l) ->
              (Syn_Binds) (l)
wrap_Binds sem (Inh_Binds) =
    (let ( ) = sem
     in  (Syn_Binds))
sem_Binds_BDecls :: (l) ->
                    ([Decl l]) ->
                    (T_Binds) (l)
sem_Binds_BDecls (loca_ :: (l)) (decls_ :: ([Decl l])) =
    (let
     in  ( ))
sem_Binds_IPBinds :: (l) ->
                     ([IPBind l]) ->
                     (T_Binds) (l)
sem_Binds_IPBinds (loca_ :: (l)) (iPBinds_ :: ([IPBind l])) =
    (let
     in  ( ))
-- BooleanFormula ----------------------------------------------
-- cata
sem_BooleanFormula :: (BooleanFormula) (l) ->
                      (T_BooleanFormula) (l)
sem_BooleanFormula (VarFormula _loca _name) =
    (sem_BooleanFormula_VarFormula _loca _name)
sem_BooleanFormula (AndFormula _loca _booleanFormulas) =
    (sem_BooleanFormula_AndFormula _loca _booleanFormulas)
sem_BooleanFormula (OrFormula _loca _booleanFormulas) =
    (sem_BooleanFormula_OrFormula _loca _booleanFormulas)
sem_BooleanFormula (ParenFormula _loca _booleanFormula) =
    (sem_BooleanFormula_ParenFormula _loca _booleanFormula)
-- semantic domain
type T_BooleanFormula l = ( )
data Inh_BooleanFormula l = Inh_BooleanFormula {}
data Syn_BooleanFormula l = Syn_BooleanFormula {}
wrap_BooleanFormula :: (T_BooleanFormula) (l) ->
                       (Inh_BooleanFormula) (l) ->
                       (Syn_BooleanFormula) (l)
wrap_BooleanFormula sem (Inh_BooleanFormula) =
    (let ( ) = sem
     in  (Syn_BooleanFormula))
sem_BooleanFormula_VarFormula :: (l) ->
                                 (Name l) ->
                                 (T_BooleanFormula) (l)
sem_BooleanFormula_VarFormula (loca_ :: (l)) (name_ :: (Name l)) =
    (let
     in  ( ))
sem_BooleanFormula_AndFormula :: (l) ->
                                 ([BooleanFormula l]) ->
                                 (T_BooleanFormula) (l)
sem_BooleanFormula_AndFormula (loca_ :: (l)) (booleanFormulas_ :: ([BooleanFormula l])) =
    (let
     in  ( ))
sem_BooleanFormula_OrFormula :: (l) ->
                                ([BooleanFormula l]) ->
                                (T_BooleanFormula) (l)
sem_BooleanFormula_OrFormula (loca_ :: (l)) (booleanFormulas_ :: ([BooleanFormula l])) =
    (let
     in  ( ))
sem_BooleanFormula_ParenFormula :: (l) ->
                                   (BooleanFormula l) ->
                                   (T_BooleanFormula) (l)
sem_BooleanFormula_ParenFormula (loca_ :: (l)) (booleanFormula_ :: (BooleanFormula l)) =
    (let
     in  ( ))
-- Boxed -------------------------------------------------------
-- cata
sem_Boxed :: Boxed ->
             T_Boxed
sem_Boxed (Boxed) =
    (sem_Boxed_Boxed)
sem_Boxed (Unboxed) =
    (sem_Boxed_Unboxed)
-- semantic domain
type T_Boxed = ( )
data Inh_Boxed = Inh_Boxed {}
data Syn_Boxed = Syn_Boxed {}
wrap_Boxed :: T_Boxed ->
              Inh_Boxed ->
              Syn_Boxed
wrap_Boxed sem (Inh_Boxed) =
    (let ( ) = sem
     in  (Syn_Boxed))
sem_Boxed_Boxed :: T_Boxed
sem_Boxed_Boxed =
    (let
     in  ( ))
sem_Boxed_Unboxed :: T_Boxed
sem_Boxed_Unboxed =
    (let
     in  ( ))
-- Bracket -----------------------------------------------------
-- cata
sem_Bracket :: (Bracket) (l) ->
               (T_Bracket) (l)
sem_Bracket (ExpBracket _loca _exp) =
    (sem_Bracket_ExpBracket _loca _exp)
sem_Bracket (PatBracket _loca _pat) =
    (sem_Bracket_PatBracket _loca _pat)
sem_Bracket (TypeBracket _loca _typ) =
    (sem_Bracket_TypeBracket _loca _typ)
sem_Bracket (DeclBracket _loca _decls) =
    (sem_Bracket_DeclBracket _loca _decls)
-- semantic domain
type T_Bracket l = ( )
data Inh_Bracket l = Inh_Bracket {}
data Syn_Bracket l = Syn_Bracket {}
wrap_Bracket :: (T_Bracket) (l) ->
                (Inh_Bracket) (l) ->
                (Syn_Bracket) (l)
wrap_Bracket sem (Inh_Bracket) =
    (let ( ) = sem
     in  (Syn_Bracket))
sem_Bracket_ExpBracket :: (l) ->
                          (Exp l) ->
                          (T_Bracket) (l)
sem_Bracket_ExpBracket (loca_ :: (l)) (exp_ :: (Exp l)) =
    (let
     in  ( ))
sem_Bracket_PatBracket :: (l) ->
                          (Pat l) ->
                          (T_Bracket) (l)
sem_Bracket_PatBracket (loca_ :: (l)) (pat_ :: (Pat l)) =
    (let
     in  ( ))
sem_Bracket_TypeBracket :: (l) ->
                           (Type l) ->
                           (T_Bracket) (l)
sem_Bracket_TypeBracket (loca_ :: (l)) (typ_ :: (Type l)) =
    (let
     in  ( ))
sem_Bracket_DeclBracket :: (l) ->
                           ([Decl l]) ->
                           (T_Bracket) (l)
sem_Bracket_DeclBracket (loca_ :: (l)) (decls_ :: ([Decl l])) =
    (let
     in  ( ))
-- CName -------------------------------------------------------
-- cata
sem_CName :: (CName) (l) ->
             (T_CName) (l)
sem_CName (VarName _loca _varName) =
    (sem_CName_VarName _loca _varName)
sem_CName (ConName _loca _conName) =
    (sem_CName_ConName _loca _conName)
-- semantic domain
type T_CName l = ( )
data Inh_CName l = Inh_CName {}
data Syn_CName l = Syn_CName {}
wrap_CName :: (T_CName) (l) ->
              (Inh_CName) (l) ->
              (Syn_CName) (l)
wrap_CName sem (Inh_CName) =
    (let ( ) = sem
     in  (Syn_CName))
sem_CName_VarName :: (l) ->
                     (Name l) ->
                     (T_CName) (l)
sem_CName_VarName (loca_ :: (l)) (varName_ :: (Name l)) =
    (let
     in  ( ))
sem_CName_ConName :: (l) ->
                     (Name l) ->
                     (T_CName) (l)
sem_CName_ConName (loca_ :: (l)) (conName_ :: (Name l)) =
    (let
     in  ( ))
-- CallConv ----------------------------------------------------
-- cata
sem_CallConv :: (CallConv) (l) ->
                (T_CallConv) (l)
sem_CallConv (StdCall _loca) =
    (sem_CallConv_StdCall _loca)
sem_CallConv (CCall _loca) =
    (sem_CallConv_CCall _loca)
sem_CallConv (CPlusPlus _loca) =
    (sem_CallConv_CPlusPlus _loca)
sem_CallConv (DotNet _loca) =
    (sem_CallConv_DotNet _loca)
sem_CallConv (Jvm _loca) =
    (sem_CallConv_Jvm _loca)
sem_CallConv (Js _loca) =
    (sem_CallConv_Js _loca)
sem_CallConv (JavaScript _loca) =
    (sem_CallConv_JavaScript _loca)
sem_CallConv (CApi _loca) =
    (sem_CallConv_CApi _loca)
-- semantic domain
type T_CallConv l = ( )
data Inh_CallConv l = Inh_CallConv {}
data Syn_CallConv l = Syn_CallConv {}
wrap_CallConv :: (T_CallConv) (l) ->
                 (Inh_CallConv) (l) ->
                 (Syn_CallConv) (l)
wrap_CallConv sem (Inh_CallConv) =
    (let ( ) = sem
     in  (Syn_CallConv))
sem_CallConv_StdCall :: (l) ->
                        (T_CallConv) (l)
sem_CallConv_StdCall (loca_ :: (l)) =
    (let
     in  ( ))
sem_CallConv_CCall :: (l) ->
                      (T_CallConv) (l)
sem_CallConv_CCall (loca_ :: (l)) =
    (let
     in  ( ))
sem_CallConv_CPlusPlus :: (l) ->
                          (T_CallConv) (l)
sem_CallConv_CPlusPlus (loca_ :: (l)) =
    (let
     in  ( ))
sem_CallConv_DotNet :: (l) ->
                       (T_CallConv) (l)
sem_CallConv_DotNet (loca_ :: (l)) =
    (let
     in  ( ))
sem_CallConv_Jvm :: (l) ->
                    (T_CallConv) (l)
sem_CallConv_Jvm (loca_ :: (l)) =
    (let
     in  ( ))
sem_CallConv_Js :: (l) ->
                   (T_CallConv) (l)
sem_CallConv_Js (loca_ :: (l)) =
    (let
     in  ( ))
sem_CallConv_JavaScript :: (l) ->
                           (T_CallConv) (l)
sem_CallConv_JavaScript (loca_ :: (l)) =
    (let
     in  ( ))
sem_CallConv_CApi :: (l) ->
                     (T_CallConv) (l)
sem_CallConv_CApi (loca_ :: (l)) =
    (let
     in  ( ))
-- ClassDecl ---------------------------------------------------
-- cata
sem_ClassDecl :: (ClassDecl) (l) ->
                 (T_ClassDecl) (l)
sem_ClassDecl (ClsDecl _loca _decl) =
    (sem_ClassDecl_ClsDecl _loca _decl)
sem_ClassDecl (ClsDataFam _loca _mContext _declHead _mResultSig) =
    (sem_ClassDecl_ClsDataFam _loca _mContext _declHead _mResultSig)
sem_ClassDecl (ClsTyFam _loca _declHead _mResultSig _mInjectivityInfo) =
    (sem_ClassDecl_ClsTyFam _loca _declHead _mResultSig _mInjectivityInfo)
sem_ClassDecl (ClsTyDef _loca _typeEqn) =
    (sem_ClassDecl_ClsTyDef _loca _typeEqn)
sem_ClassDecl (ClsDefSig _loca _name _typ) =
    (sem_ClassDecl_ClsDefSig _loca _name _typ)
-- semantic domain
type T_ClassDecl l = ( )
data Inh_ClassDecl l = Inh_ClassDecl {}
data Syn_ClassDecl l = Syn_ClassDecl {}
wrap_ClassDecl :: (T_ClassDecl) (l) ->
                  (Inh_ClassDecl) (l) ->
                  (Syn_ClassDecl) (l)
wrap_ClassDecl sem (Inh_ClassDecl) =
    (let ( ) = sem
     in  (Syn_ClassDecl))
sem_ClassDecl_ClsDecl :: (l) ->
                         (Decl l) ->
                         (T_ClassDecl) (l)
sem_ClassDecl_ClsDecl (loca_ :: (l)) (decl_ :: (Decl l)) =
    (let
     in  ( ))
sem_ClassDecl_ClsDataFam :: (l) ->
                            (Maybe (Context l)) ->
                            (DeclHead l) ->
                            (Maybe (ResultSig l)) ->
                            (T_ClassDecl) (l)
sem_ClassDecl_ClsDataFam (loca_ :: (l)) (mContext_ :: (Maybe (Context l))) (declHead_ :: (DeclHead l)) (mResultSig_ :: (Maybe (ResultSig l))) =
    (let
     in  ( ))
sem_ClassDecl_ClsTyFam :: (l) ->
                          (DeclHead l) ->
                          (Maybe (ResultSig l)) ->
                          (Maybe (InjectivityInfo l)) ->
                          (T_ClassDecl) (l)
sem_ClassDecl_ClsTyFam (loca_ :: (l)) (declHead_ :: (DeclHead l)) (mResultSig_ :: (Maybe (ResultSig l))) (mInjectivityInfo_ :: (Maybe (InjectivityInfo l))) =
    (let
     in  ( ))
sem_ClassDecl_ClsTyDef :: (l) ->
                          (TypeEqn l) ->
                          (T_ClassDecl) (l)
sem_ClassDecl_ClsTyDef (loca_ :: (l)) (typeEqn_ :: (TypeEqn l)) =
    (let
     in  ( ))
sem_ClassDecl_ClsDefSig :: (l) ->
                           (Name l) ->
                           (Type l) ->
                           (T_ClassDecl) (l)
sem_ClassDecl_ClsDefSig (loca_ :: (l)) (name_ :: (Name l)) (typ_ :: (Type l)) =
    (let
     in  ( ))
-- ConDecl -----------------------------------------------------
-- cata
sem_ConDecl :: (ConDecl) (l) ->
               (T_ConDecl) (l)
sem_ConDecl (ConDecl _loca _name _types) =
    (sem_ConDecl_ConDecl _loca _name _types)
sem_ConDecl (InfixConDecl _loca _typ1 _name _typ2) =
    (sem_ConDecl_InfixConDecl _loca _typ1 _name _typ2)
sem_ConDecl (RecDecl _loca _name _fieldDecls) =
    (sem_ConDecl_RecDecl _loca _name _fieldDecls)
-- semantic domain
type T_ConDecl l = ( )
data Inh_ConDecl l = Inh_ConDecl {}
data Syn_ConDecl l = Syn_ConDecl {}
wrap_ConDecl :: (T_ConDecl) (l) ->
                (Inh_ConDecl) (l) ->
                (Syn_ConDecl) (l)
wrap_ConDecl sem (Inh_ConDecl) =
    (let ( ) = sem
     in  (Syn_ConDecl))
sem_ConDecl_ConDecl :: (l) ->
                       (Name l) ->
                       ([Type l]) ->
                       (T_ConDecl) (l)
sem_ConDecl_ConDecl (loca_ :: (l)) (name_ :: (Name l)) (types_ :: ([Type l])) =
    (let
     in  ( ))
sem_ConDecl_InfixConDecl :: (l) ->
                            (Type l) ->
                            (Name l) ->
                            (Type l) ->
                            (T_ConDecl) (l)
sem_ConDecl_InfixConDecl (loca_ :: (l)) (typ1_ :: (Type l)) (name_ :: (Name l)) (typ2_ :: (Type l)) =
    (let
     in  ( ))
sem_ConDecl_RecDecl :: (l) ->
                       (Name l) ->
                       ([FieldDecl l]) ->
                       (T_ConDecl) (l)
sem_ConDecl_RecDecl (loca_ :: (l)) (name_ :: (Name l)) (fieldDecls_ :: ([FieldDecl l])) =
    (let
     in  ( ))
-- Context -----------------------------------------------------
-- cata
sem_Context :: (Context) (l) ->
               (T_Context) (l)
sem_Context (CxSingle _loca _assertion) =
    (sem_Context_CxSingle _loca _assertion)
sem_Context (CxTuple _loca _assertionTuple) =
    (sem_Context_CxTuple _loca _assertionTuple)
sem_Context (CxEmpty _loca) =
    (sem_Context_CxEmpty _loca)
-- semantic domain
type T_Context l = ( )
data Inh_Context l = Inh_Context {}
data Syn_Context l = Syn_Context {}
wrap_Context :: (T_Context) (l) ->
                (Inh_Context) (l) ->
                (Syn_Context) (l)
wrap_Context sem (Inh_Context) =
    (let ( ) = sem
     in  (Syn_Context))
sem_Context_CxSingle :: (l) ->
                        (Asst l) ->
                        (T_Context) (l)
sem_Context_CxSingle (loca_ :: (l)) (assertion_ :: (Asst l)) =
    (let
     in  ( ))
sem_Context_CxTuple :: (l) ->
                       ([Asst l]) ->
                       (T_Context) (l)
sem_Context_CxTuple (loca_ :: (l)) (assertionTuple_ :: ([Asst l])) =
    (let
     in  ( ))
sem_Context_CxEmpty :: (l) ->
                       (T_Context) (l)
sem_Context_CxEmpty (loca_ :: (l)) =
    (let
     in  ( ))
-- DataOrNew ---------------------------------------------------
-- cata
sem_DataOrNew :: (DataOrNew) (l) ->
                 (T_DataOrNew) (l)
sem_DataOrNew (DataType _loca) =
    (sem_DataOrNew_DataType _loca)
sem_DataOrNew (NewType _loca) =
    (sem_DataOrNew_NewType _loca)
-- semantic domain
type T_DataOrNew l = ( )
data Inh_DataOrNew l = Inh_DataOrNew {}
data Syn_DataOrNew l = Syn_DataOrNew {}
wrap_DataOrNew :: (T_DataOrNew) (l) ->
                  (Inh_DataOrNew) (l) ->
                  (Syn_DataOrNew) (l)
wrap_DataOrNew sem (Inh_DataOrNew) =
    (let ( ) = sem
     in  (Syn_DataOrNew))
sem_DataOrNew_DataType :: (l) ->
                          (T_DataOrNew) (l)
sem_DataOrNew_DataType (loca_ :: (l)) =
    (let
     in  ( ))
sem_DataOrNew_NewType :: (l) ->
                         (T_DataOrNew) (l)
sem_DataOrNew_NewType (loca_ :: (l)) =
    (let
     in  ( ))
-- Decl --------------------------------------------------------
-- cata
sem_Decl :: (Decl) (l) ->
            (T_Decl) (l)
sem_Decl (TypeDecl _loca _declHead _typ) =
    (sem_Decl_TypeDecl _loca _declHead _typ)
sem_Decl (TypeFamDecl _loca _declHead _mResultSig _mInjectivityInfo) =
    (sem_Decl_TypeFamDecl _loca _declHead _mResultSig _mInjectivityInfo)
sem_Decl (ClosedTypeFamDecl _loca _declHead _mResultSig _mInjectivityInfo _typeEqns) =
    (sem_Decl_ClosedTypeFamDecl _loca _declHead _mResultSig _mInjectivityInfo _typeEqns)
sem_Decl (DataDecl _loca _dataOrNew _mContext _declHead _qualConDecls _derivings) =
    (sem_Decl_DataDecl _loca _dataOrNew _mContext _declHead _qualConDecls _derivings)
sem_Decl (GDataDecl _loca _dataOrNew _mContext _declHead _mKind _gadtDecls _derivings) =
    (sem_Decl_GDataDecl _loca _dataOrNew _mContext _declHead _mKind _gadtDecls _derivings)
sem_Decl (DataFamDecl _loca _mContext _declHead _mResultSig) =
    (sem_Decl_DataFamDecl _loca _mContext _declHead _mResultSig)
sem_Decl (TypeInsDecl _loca _leftType _rightType) =
    (sem_Decl_TypeInsDecl _loca _leftType _rightType)
sem_Decl (DataInsDecl _loca _dataOrNew _typ _qualConDecls _derivings) =
    (sem_Decl_DataInsDecl _loca _dataOrNew _typ _qualConDecls _derivings)
sem_Decl (GDataInsDecl _loca _dataOrNew _typ _mKind _gadtDecls _derivings) =
    (sem_Decl_GDataInsDecl _loca _dataOrNew _typ _mKind _gadtDecls _derivings)
sem_Decl (ClassDecl _loca _mContext _declHead _funDeps _mClassDecls) =
    (sem_Decl_ClassDecl _loca _mContext _declHead _funDeps _mClassDecls)
sem_Decl (InstDecl _loca _mOverlap _instRule _mInstDecls) =
    (sem_Decl_InstDecl _loca _mOverlap _instRule _mInstDecls)
sem_Decl (DerivDecl _loca _mDerivStrategy _mOverlap _instRule) =
    (sem_Decl_DerivDecl _loca _mDerivStrategy _mOverlap _instRule)
sem_Decl (InfixDecl _loca _assoc _mFixity _ops) =
    (sem_Decl_InfixDecl _loca _assoc _mFixity _ops)
sem_Decl (DefaultDecl _loca _types) =
    (sem_Decl_DefaultDecl _loca _types)
sem_Decl (SpliceDecl _loca _spliceExp) =
    (sem_Decl_SpliceDecl _loca _spliceExp)
sem_Decl (TypeSig _loca _names _typ) =
    (sem_Decl_TypeSig _loca _names _typ)
sem_Decl (PatSynSig _loca _names _mTyVarBind _mContextLeft _mContextRight _typ) =
    (sem_Decl_PatSynSig _loca _names _mTyVarBind _mContextLeft _mContextRight _typ)
sem_Decl (FunBind _loca _matches) =
    (sem_Decl_FunBind _loca _matches)
sem_Decl (PatBind _loca _pat _rhs _mBinds) =
    (sem_Decl_PatBind _loca _pat _rhs _mBinds)
sem_Decl (PatSyn _loca _patLeft _patRight _patternSynDirection) =
    (sem_Decl_PatSyn _loca _patLeft _patRight _patternSynDirection)
sem_Decl (ForImp _loca _callConv _mSafety _mStr _name _typ) =
    (sem_Decl_ForImp _loca _callConv _mSafety _mStr _name _typ)
sem_Decl (ForExp _loca _callConv _mStr _name _typ) =
    (sem_Decl_ForExp _loca _callConv _mStr _name _typ)
sem_Decl (RulePragmaDecl _loca _rules) =
    (sem_Decl_RulePragmaDecl _loca _rules)
sem_Decl (DeprPragmaDecl _loca _nameStrList) =
    (sem_Decl_DeprPragmaDecl _loca _nameStrList)
sem_Decl (WarnPragmaDecl _loca _nameStrList) =
    (sem_Decl_WarnPragmaDecl _loca _nameStrList)
sem_Decl (InlineSig _loca _b _mActivation _qName) =
    (sem_Decl_InlineSig _loca _b _mActivation _qName)
sem_Decl (InlineConlikeSig _loca _mActivation _qName) =
    (sem_Decl_InlineConlikeSig _loca _mActivation _qName)
sem_Decl (SpecSig _loca _mActivation _qName _types) =
    (sem_Decl_SpecSig _loca _mActivation _qName _types)
sem_Decl (SpecInlineSig _loca _b _mActivation _qName _types) =
    (sem_Decl_SpecInlineSig _loca _b _mActivation _qName _types)
sem_Decl (InstSig _loca _instRule) =
    (sem_Decl_InstSig _loca _instRule)
sem_Decl (AnnPragma _loca _annotation) =
    (sem_Decl_AnnPragma _loca _annotation)
sem_Decl (MinimalPragma _loca _mBooleanFormula) =
    (sem_Decl_MinimalPragma _loca _mBooleanFormula)
sem_Decl (RoleAnnotDecl _loca _qName _roles) =
    (sem_Decl_RoleAnnotDecl _loca _qName _roles)
sem_Decl (CompletePragma _loca _names _mQName) =
    (sem_Decl_CompletePragma _loca _names _mQName)
-- semantic domain
type T_Decl l = ( )
data Inh_Decl l = Inh_Decl {}
data Syn_Decl l = Syn_Decl {}
wrap_Decl :: (T_Decl) (l) ->
             (Inh_Decl) (l) ->
             (Syn_Decl) (l)
wrap_Decl sem (Inh_Decl) =
    (let ( ) = sem
     in  (Syn_Decl))
sem_Decl_TypeDecl :: (l) ->
                     (DeclHead l) ->
                     (Type l) ->
                     (T_Decl) (l)
sem_Decl_TypeDecl (loca_ :: (l)) (declHead_ :: (DeclHead l)) (typ_ :: (Type l)) =
    (let
     in  ( ))
sem_Decl_TypeFamDecl :: (l) ->
                        (DeclHead l) ->
                        (Maybe (ResultSig l)) ->
                        (Maybe (InjectivityInfo l)) ->
                        (T_Decl) (l)
sem_Decl_TypeFamDecl (loca_ :: (l)) (declHead_ :: (DeclHead l)) (mResultSig_ :: (Maybe (ResultSig l))) (mInjectivityInfo_ :: (Maybe (InjectivityInfo l))) =
    (let
     in  ( ))
sem_Decl_ClosedTypeFamDecl :: (l) ->
                              (DeclHead l) ->
                              (Maybe (ResultSig l)) ->
                              (Maybe (InjectivityInfo l)) ->
                              ([TypeEqn l]) ->
                              (T_Decl) (l)
sem_Decl_ClosedTypeFamDecl (loca_ :: (l)) (declHead_ :: (DeclHead l)) (mResultSig_ :: (Maybe (ResultSig l))) (mInjectivityInfo_ :: (Maybe (InjectivityInfo l))) (typeEqns_ :: ([TypeEqn l])) =
    (let
     in  ( ))
sem_Decl_DataDecl :: (l) ->
                     (DataOrNew l) ->
                     (Maybe (Context l)) ->
                     (DeclHead l) ->
                     ([QualConDecl l]) ->
                     ([Deriving l]) ->
                     (T_Decl) (l)
sem_Decl_DataDecl (loca_ :: (l)) (dataOrNew_ :: (DataOrNew l)) (mContext_ :: (Maybe (Context l))) (declHead_ :: (DeclHead l)) (qualConDecls_ :: ([QualConDecl l])) (derivings_ :: ([Deriving l])) =
    (let
     in  ( ))
sem_Decl_GDataDecl :: (l) ->
                      (DataOrNew l) ->
                      (Maybe (Context l)) ->
                      (DeclHead l) ->
                      (Maybe (Kind l)) ->
                      ([GadtDecl l]) ->
                      ([Deriving l]) ->
                      (T_Decl) (l)
sem_Decl_GDataDecl (loca_ :: (l)) (dataOrNew_ :: (DataOrNew l)) (mContext_ :: (Maybe (Context l))) (declHead_ :: (DeclHead l)) (mKind_ :: (Maybe (Kind l))) (gadtDecls_ :: ([GadtDecl l])) (derivings_ :: ([Deriving l])) =
    (let
     in  ( ))
sem_Decl_DataFamDecl :: (l) ->
                        (Maybe (Context l)) ->
                        (DeclHead l) ->
                        (Maybe (ResultSig l)) ->
                        (T_Decl) (l)
sem_Decl_DataFamDecl (loca_ :: (l)) (mContext_ :: (Maybe (Context l))) (declHead_ :: (DeclHead l)) (mResultSig_ :: (Maybe (ResultSig l))) =
    (let
     in  ( ))
sem_Decl_TypeInsDecl :: (l) ->
                        (Type l) ->
                        (Type l) ->
                        (T_Decl) (l)
sem_Decl_TypeInsDecl (loca_ :: (l)) (leftType_ :: (Type l)) (rightType_ :: (Type l)) =
    (let
     in  ( ))
sem_Decl_DataInsDecl :: (l) ->
                        (DataOrNew l) ->
                        (Type l) ->
                        ([QualConDecl l]) ->
                        ([Deriving l]) ->
                        (T_Decl) (l)
sem_Decl_DataInsDecl (loca_ :: (l)) (dataOrNew_ :: (DataOrNew l)) (typ_ :: (Type l)) (qualConDecls_ :: ([QualConDecl l])) (derivings_ :: ([Deriving l])) =
    (let
     in  ( ))
sem_Decl_GDataInsDecl :: (l) ->
                         (DataOrNew l) ->
                         (Type l) ->
                         (Maybe (Kind l)) ->
                         ([GadtDecl l]) ->
                         ([Deriving l]) ->
                         (T_Decl) (l)
sem_Decl_GDataInsDecl (loca_ :: (l)) (dataOrNew_ :: (DataOrNew l)) (typ_ :: (Type l)) (mKind_ :: (Maybe (Kind l))) (gadtDecls_ :: ([GadtDecl l])) (derivings_ :: ([Deriving l])) =
    (let
     in  ( ))
sem_Decl_ClassDecl :: (l) ->
                      (Maybe (Context l)) ->
                      (DeclHead l) ->
                      ([FunDep l]) ->
                      (Maybe [ClassDecl l]) ->
                      (T_Decl) (l)
sem_Decl_ClassDecl (loca_ :: (l)) (mContext_ :: (Maybe (Context l))) (declHead_ :: (DeclHead l)) (funDeps_ :: ([FunDep l])) (mClassDecls_ :: (Maybe [ClassDecl l])) =
    (let
     in  ( ))
sem_Decl_InstDecl :: (l) ->
                     (Maybe (Overlap l)) ->
                     (InstRule l) ->
                     (Maybe [InstDecl l]) ->
                     (T_Decl) (l)
sem_Decl_InstDecl (loca_ :: (l)) (mOverlap_ :: (Maybe (Overlap l))) (instRule_ :: (InstRule l)) (mInstDecls_ :: (Maybe [InstDecl l])) =
    (let
     in  ( ))
sem_Decl_DerivDecl :: (l) ->
                      (Maybe (DerivStrategy l)) ->
                      (Maybe (Overlap l)) ->
                      (InstRule l) ->
                      (T_Decl) (l)
sem_Decl_DerivDecl (loca_ :: (l)) (mDerivStrategy_ :: (Maybe (DerivStrategy l))) (mOverlap_ :: (Maybe (Overlap l))) (instRule_ :: (InstRule l)) =
    (let
     in  ( ))
sem_Decl_InfixDecl :: (l) ->
                      (Assoc l) ->
                      (Maybe Int) ->
                      ([Op l]) ->
                      (T_Decl) (l)
sem_Decl_InfixDecl (loca_ :: (l)) (assoc_ :: (Assoc l)) (mFixity_ :: (Maybe Int)) (ops_ :: ([Op l])) =
    (let
     in  ( ))
sem_Decl_DefaultDecl :: (l) ->
                        ([Type l]) ->
                        (T_Decl) (l)
sem_Decl_DefaultDecl (loca_ :: (l)) (types_ :: ([Type l])) =
    (let
     in  ( ))
sem_Decl_SpliceDecl :: (l) ->
                       (Exp l) ->
                       (T_Decl) (l)
sem_Decl_SpliceDecl (loca_ :: (l)) (spliceExp_ :: (Exp l)) =
    (let
     in  ( ))
sem_Decl_TypeSig :: (l) ->
                    ([Name l]) ->
                    (Type l) ->
                    (T_Decl) (l)
sem_Decl_TypeSig (loca_ :: (l)) (names_ :: ([Name l])) (typ_ :: (Type l)) =
    (let
     in  ( ))
sem_Decl_PatSynSig :: (l) ->
                      ([Name l]) ->
                      (Maybe [TyVarBind l]) ->
                      (Maybe (Context l)) ->
                      (Maybe (Context l)) ->
                      (Type l) ->
                      (T_Decl) (l)
sem_Decl_PatSynSig (loca_ :: (l)) (names_ :: ([Name l])) (mTyVarBind_ :: (Maybe [TyVarBind l])) (mContextLeft_ :: (Maybe (Context l))) (mContextRight_ :: (Maybe (Context l))) (typ_ :: (Type l)) =
    (let
     in  ( ))
sem_Decl_FunBind :: (l) ->
                    ([Match l]) ->
                    (T_Decl) (l)
sem_Decl_FunBind (loca_ :: (l)) (matches_ :: ([Match l])) =
    (let
     in  ( ))
sem_Decl_PatBind :: (l) ->
                    (Pat l) ->
                    (Rhs l) ->
                    (Maybe (Binds l)) ->
                    (T_Decl) (l)
sem_Decl_PatBind (loca_ :: (l)) (pat_ :: (Pat l)) (rhs_ :: (Rhs l)) (mBinds_ :: (Maybe (Binds l))) =
    (let
     in  ( ))
sem_Decl_PatSyn :: (l) ->
                   (Pat l) ->
                   (Pat l) ->
                   (PatternSynDirection l) ->
                   (T_Decl) (l)
sem_Decl_PatSyn (loca_ :: (l)) (patLeft_ :: (Pat l)) (patRight_ :: (Pat l)) (patternSynDirection_ :: (PatternSynDirection l)) =
    (let
     in  ( ))
sem_Decl_ForImp :: (l) ->
                   (CallConv l) ->
                   (Maybe (Safety l)) ->
                   (Maybe String) ->
                   (Name l) ->
                   (Type l) ->
                   (T_Decl) (l)
sem_Decl_ForImp (loca_ :: (l)) (callConv_ :: (CallConv l)) (mSafety_ :: (Maybe (Safety l))) (mStr_ :: (Maybe String)) (name_ :: (Name l)) (typ_ :: (Type l)) =
    (let
     in  ( ))
sem_Decl_ForExp :: (l) ->
                   (CallConv l) ->
                   (Maybe String) ->
                   (Name l) ->
                   (Type l) ->
                   (T_Decl) (l)
sem_Decl_ForExp (loca_ :: (l)) (callConv_ :: (CallConv l)) (mStr_ :: (Maybe String)) (name_ :: (Name l)) (typ_ :: (Type l)) =
    (let
     in  ( ))
sem_Decl_RulePragmaDecl :: (l) ->
                           ([Rule l]) ->
                           (T_Decl) (l)
sem_Decl_RulePragmaDecl (loca_ :: (l)) (rules_ :: ([Rule l])) =
    (let
     in  ( ))
sem_Decl_DeprPragmaDecl :: (l) ->
                           ([([Name l], String)]) ->
                           (T_Decl) (l)
sem_Decl_DeprPragmaDecl (loca_ :: (l)) (nameStrList_ :: ([([Name l], String)])) =
    (let
     in  ( ))
sem_Decl_WarnPragmaDecl :: (l) ->
                           ([([Name l], String)]) ->
                           (T_Decl) (l)
sem_Decl_WarnPragmaDecl (loca_ :: (l)) (nameStrList_ :: ([([Name l], String)])) =
    (let
     in  ( ))
sem_Decl_InlineSig :: (l) ->
                      (Bool) ->
                      (Maybe (Activation l)) ->
                      (QName l) ->
                      (T_Decl) (l)
sem_Decl_InlineSig (loca_ :: (l)) (b_ :: (Bool)) (mActivation_ :: (Maybe (Activation l))) (qName_ :: (QName l)) =
    (let
     in  ( ))
sem_Decl_InlineConlikeSig :: (l) ->
                             (Maybe (Activation l)) ->
                             (QName l) ->
                             (T_Decl) (l)
sem_Decl_InlineConlikeSig (loca_ :: (l)) (mActivation_ :: (Maybe (Activation l))) (qName_ :: (QName l)) =
    (let
     in  ( ))
sem_Decl_SpecSig :: (l) ->
                    (Maybe (Activation l)) ->
                    (QName l) ->
                    ([Type l]) ->
                    (T_Decl) (l)
sem_Decl_SpecSig (loca_ :: (l)) (mActivation_ :: (Maybe (Activation l))) (qName_ :: (QName l)) (types_ :: ([Type l])) =
    (let
     in  ( ))
sem_Decl_SpecInlineSig :: (l) ->
                          (Bool) ->
                          (Maybe (Activation l)) ->
                          (QName l) ->
                          ([Type l]) ->
                          (T_Decl) (l)
sem_Decl_SpecInlineSig (loca_ :: (l)) (b_ :: (Bool)) (mActivation_ :: (Maybe (Activation l))) (qName_ :: (QName l)) (types_ :: ([Type l])) =
    (let
     in  ( ))
sem_Decl_InstSig :: (l) ->
                    (InstRule l) ->
                    (T_Decl) (l)
sem_Decl_InstSig (loca_ :: (l)) (instRule_ :: (InstRule l)) =
    (let
     in  ( ))
sem_Decl_AnnPragma :: (l) ->
                      (Annotation l) ->
                      (T_Decl) (l)
sem_Decl_AnnPragma (loca_ :: (l)) (annotation_ :: (Annotation l)) =
    (let
     in  ( ))
sem_Decl_MinimalPragma :: (l) ->
                          (Maybe (BooleanFormula l)) ->
                          (T_Decl) (l)
sem_Decl_MinimalPragma (loca_ :: (l)) (mBooleanFormula_ :: (Maybe (BooleanFormula l))) =
    (let
     in  ( ))
sem_Decl_RoleAnnotDecl :: (l) ->
                          (QName l) ->
                          ([Role l]) ->
                          (T_Decl) (l)
sem_Decl_RoleAnnotDecl (loca_ :: (l)) (qName_ :: (QName l)) (roles_ :: ([Role l])) =
    (let
     in  ( ))
sem_Decl_CompletePragma :: (l) ->
                           ([Name l]) ->
                           (Maybe (QName l)) ->
                           (T_Decl) (l)
sem_Decl_CompletePragma (loca_ :: (l)) (names_ :: ([Name l])) (mQName_ :: (Maybe (QName l))) =
    (let
     in  ( ))
-- DeclHead ----------------------------------------------------
-- cata
sem_DeclHead :: (DeclHead) (l) ->
                (T_DeclHead) (l)
sem_DeclHead (DHead _loca _name) =
    (sem_DeclHead_DHead _loca _name)
sem_DeclHead (DHInfix _loca _tyVarBind _name) =
    (sem_DeclHead_DHInfix _loca _tyVarBind _name)
sem_DeclHead (DHParen _loca _declHead) =
    (sem_DeclHead_DHParen _loca _declHead)
sem_DeclHead (DHApp _loca _declHead _tyVarBind) =
    (sem_DeclHead_DHApp _loca _declHead _tyVarBind)
-- semantic domain
type T_DeclHead l = ( )
data Inh_DeclHead l = Inh_DeclHead {}
data Syn_DeclHead l = Syn_DeclHead {}
wrap_DeclHead :: (T_DeclHead) (l) ->
                 (Inh_DeclHead) (l) ->
                 (Syn_DeclHead) (l)
wrap_DeclHead sem (Inh_DeclHead) =
    (let ( ) = sem
     in  (Syn_DeclHead))
sem_DeclHead_DHead :: (l) ->
                      (Name l) ->
                      (T_DeclHead) (l)
sem_DeclHead_DHead (loca_ :: (l)) (name_ :: (Name l)) =
    (let
     in  ( ))
sem_DeclHead_DHInfix :: (l) ->
                        (TyVarBind l) ->
                        (Name l) ->
                        (T_DeclHead) (l)
sem_DeclHead_DHInfix (loca_ :: (l)) (tyVarBind_ :: (TyVarBind l)) (name_ :: (Name l)) =
    (let
     in  ( ))
sem_DeclHead_DHParen :: (l) ->
                        (DeclHead l) ->
                        (T_DeclHead) (l)
sem_DeclHead_DHParen (loca_ :: (l)) (declHead_ :: (DeclHead l)) =
    (let
     in  ( ))
sem_DeclHead_DHApp :: (l) ->
                      (DeclHead l) ->
                      (TyVarBind l) ->
                      (T_DeclHead) (l)
sem_DeclHead_DHApp (loca_ :: (l)) (declHead_ :: (DeclHead l)) (tyVarBind_ :: (TyVarBind l)) =
    (let
     in  ( ))
-- DerivStrategy -----------------------------------------------
-- cata
sem_DerivStrategy :: (DerivStrategy) (l) ->
                     (T_DerivStrategy) (l)
sem_DerivStrategy (DerivStock _loca) =
    (sem_DerivStrategy_DerivStock _loca)
sem_DerivStrategy (DerivAnyclass _loca) =
    (sem_DerivStrategy_DerivAnyclass _loca)
sem_DerivStrategy (DerivNewtype _loca) =
    (sem_DerivStrategy_DerivNewtype _loca)
-- semantic domain
type T_DerivStrategy l = ( )
data Inh_DerivStrategy l = Inh_DerivStrategy {}
data Syn_DerivStrategy l = Syn_DerivStrategy {}
wrap_DerivStrategy :: (T_DerivStrategy) (l) ->
                      (Inh_DerivStrategy) (l) ->
                      (Syn_DerivStrategy) (l)
wrap_DerivStrategy sem (Inh_DerivStrategy) =
    (let ( ) = sem
     in  (Syn_DerivStrategy))
sem_DerivStrategy_DerivStock :: (l) ->
                                (T_DerivStrategy) (l)
sem_DerivStrategy_DerivStock (loca_ :: (l)) =
    (let
     in  ( ))
sem_DerivStrategy_DerivAnyclass :: (l) ->
                                   (T_DerivStrategy) (l)
sem_DerivStrategy_DerivAnyclass (loca_ :: (l)) =
    (let
     in  ( ))
sem_DerivStrategy_DerivNewtype :: (l) ->
                                  (T_DerivStrategy) (l)
sem_DerivStrategy_DerivNewtype (loca_ :: (l)) =
    (let
     in  ( ))
-- Deriving ----------------------------------------------------
-- cata
sem_Deriving :: (Deriving) (l) ->
                (T_Deriving) (l)
sem_Deriving (Deriving _loca _mDerivStrategy _instRules) =
    (sem_Deriving_Deriving _loca _mDerivStrategy _instRules)
-- semantic domain
type T_Deriving l = ( )
data Inh_Deriving l = Inh_Deriving {}
data Syn_Deriving l = Syn_Deriving {}
wrap_Deriving :: (T_Deriving) (l) ->
                 (Inh_Deriving) (l) ->
                 (Syn_Deriving) (l)
wrap_Deriving sem (Inh_Deriving) =
    (let ( ) = sem
     in  (Syn_Deriving))
sem_Deriving_Deriving :: (l) ->
                         (Maybe (DerivStrategy l)) ->
                         ([InstRule l]) ->
                         (T_Deriving) (l)
sem_Deriving_Deriving (loca_ :: (l)) (mDerivStrategy_ :: (Maybe (DerivStrategy l))) (instRules_ :: ([InstRule l])) =
    (let
     in  ( ))
-- EWildcard ---------------------------------------------------
-- cata
sem_EWildcard :: (EWildcard) (l) ->
                 (T_EWildcard) (l)
sem_EWildcard (NoWildcard _loca) =
    (sem_EWildcard_NoWildcard _loca)
sem_EWildcard (EWildcard _loca _position) =
    (sem_EWildcard_EWildcard _loca _position)
-- semantic domain
type T_EWildcard l = ( )
data Inh_EWildcard l = Inh_EWildcard {}
data Syn_EWildcard l = Syn_EWildcard {}
wrap_EWildcard :: (T_EWildcard) (l) ->
                  (Inh_EWildcard) (l) ->
                  (Syn_EWildcard) (l)
wrap_EWildcard sem (Inh_EWildcard) =
    (let ( ) = sem
     in  (Syn_EWildcard))
sem_EWildcard_NoWildcard :: (l) ->
                            (T_EWildcard) (l)
sem_EWildcard_NoWildcard (loca_ :: (l)) =
    (let
     in  ( ))
sem_EWildcard_EWildcard :: (l) ->
                           (Int) ->
                           (T_EWildcard) (l)
sem_EWildcard_EWildcard (loca_ :: (l)) (position_ :: (Int)) =
    (let
     in  ( ))
-- Exp ---------------------------------------------------------
-- cata
sem_Exp :: (Exp) (l) ->
           (T_Exp) (l)
sem_Exp (Var _loca _qName) =
    (sem_Exp_Var _loca _qName)
sem_Exp (OverloadedLabel _loca _label) =
    (sem_Exp_OverloadedLabel _loca _label)
sem_Exp (IPVar _loca _iPName) =
    (sem_Exp_IPVar _loca _iPName)
sem_Exp (Con _loca _qName) =
    (sem_Exp_Con _loca _qName)
sem_Exp (Lit _loca _literal) =
    (sem_Exp_Lit _loca _literal)
sem_Exp (InfixApp _loca _exp1 _qOp _exp2) =
    (sem_Exp_InfixApp _loca _exp1 _qOp _exp2)
sem_Exp (App _loca _exp1 _exp2) =
    (sem_Exp_App _loca _exp1 _exp2)
sem_Exp (NegApp _loca _exp) =
    (sem_Exp_NegApp _loca _exp)
sem_Exp (Lambda _loca _pats _exp) =
    (sem_Exp_Lambda _loca _pats _exp)
sem_Exp (Let _loca _binds _exp) =
    (sem_Exp_Let _loca _binds _exp)
sem_Exp (If _loca _exp1 _exp2 _exp3) =
    (sem_Exp_If _loca _exp1 _exp2 _exp3)
sem_Exp (MultiIf _loca _guardedRhss) =
    (sem_Exp_MultiIf _loca _guardedRhss)
sem_Exp (Case _loca _exp _alts) =
    (sem_Exp_Case _loca _exp _alts)
sem_Exp (Do _loca _stmts) =
    (sem_Exp_Do _loca _stmts)
sem_Exp (MDo _loca _stmts) =
    (sem_Exp_MDo _loca _stmts)
sem_Exp (Tuple _loca _boxed _exps) =
    (sem_Exp_Tuple _loca (sem_Boxed _boxed) _exps)
sem_Exp (UnboxedSum _loca _int1 _int2 _exp) =
    (sem_Exp_UnboxedSum _loca _int1 _int2 _exp)
sem_Exp (TupleSection _loca _boxed _mExps) =
    (sem_Exp_TupleSection _loca (sem_Boxed _boxed) _mExps)
sem_Exp (List _loca _exps) =
    (sem_Exp_List _loca _exps)
sem_Exp (ParArray _loca _exps) =
    (sem_Exp_ParArray _loca _exps)
sem_Exp (Paren _loca _exp) =
    (sem_Exp_Paren _loca _exp)
sem_Exp (LeftSection _loca _exp _qOp) =
    (sem_Exp_LeftSection _loca _exp _qOp)
sem_Exp (RightSection _loca _qOp _exp) =
    (sem_Exp_RightSection _loca _qOp _exp)
sem_Exp (RecConstr _loca _qName _fieldUpdates) =
    (sem_Exp_RecConstr _loca _qName _fieldUpdates)
sem_Exp (RecUpdate _loca _exp _fieldUpdates) =
    (sem_Exp_RecUpdate _loca _exp _fieldUpdates)
sem_Exp (EnumFrom _loca _exp) =
    (sem_Exp_EnumFrom _loca _exp)
sem_Exp (EnumFromTo _loca _exp1 _exp2) =
    (sem_Exp_EnumFromTo _loca _exp1 _exp2)
sem_Exp (EnumFromThen _loca _exp1 _exp2) =
    (sem_Exp_EnumFromThen _loca _exp1 _exp2)
sem_Exp (EnumFromThenTo _loca _exp1 _exp2 _exp3) =
    (sem_Exp_EnumFromThenTo _loca _exp1 _exp2 _exp3)
sem_Exp (ParArrayFromTo _loca _exp1 _exp2) =
    (sem_Exp_ParArrayFromTo _loca _exp1 _exp2)
sem_Exp (ParArrayFromThenTo _loca _exp1 _exp2 _exp3) =
    (sem_Exp_ParArrayFromThenTo _loca _exp1 _exp2 _exp3)
sem_Exp (ListComp _loca _exp _qualStmts) =
    (sem_Exp_ListComp _loca _exp _qualStmts)
sem_Exp (ParComp _loca _exp _qualStmtsList) =
    (sem_Exp_ParComp _loca _exp _qualStmtsList)
sem_Exp (ParArrayComp _loca _exp _qualStmtsList) =
    (sem_Exp_ParArrayComp _loca _exp _qualStmtsList)
sem_Exp (ExpTypeSig _loca _exp _typ) =
    (sem_Exp_ExpTypeSig _loca _exp _typ)
sem_Exp (VarQuote _loca _qName) =
    (sem_Exp_VarQuote _loca _qName)
sem_Exp (TypQuote _loca _qName) =
    (sem_Exp_TypQuote _loca _qName)
sem_Exp (BracketExp _loca _bracket) =
    (sem_Exp_BracketExp _loca _bracket)
sem_Exp (SpliceExp _loca _splice) =
    (sem_Exp_SpliceExp _loca _splice)
sem_Exp (QuasiQuote _loca _qqName _qqString) =
    (sem_Exp_QuasiQuote _loca _qqName _qqString)
sem_Exp (TypeApp _loca _typ) =
    (sem_Exp_TypeApp _loca _typ)
sem_Exp (XTag _loca _xName _xAttrs _mExp _exps) =
    (sem_Exp_XTag _loca _xName _xAttrs _mExp _exps)
sem_Exp (XETag _loca _xName _xAttrs _mExp) =
    (sem_Exp_XETag _loca _xName _xAttrs _mExp)
sem_Exp (XPcdata _loca _pcData) =
    (sem_Exp_XPcdata _loca _pcData)
sem_Exp (XExpTag _loca _exp) =
    (sem_Exp_XExpTag _loca _exp)
sem_Exp (XChildTag _loca _exps) =
    (sem_Exp_XChildTag _loca _exps)
sem_Exp (CorePragma _loca _str _exp) =
    (sem_Exp_CorePragma _loca _str _exp)
sem_Exp (SCCPragma _loca _str _exp) =
    (sem_Exp_SCCPragma _loca _str _exp)
sem_Exp (GenPragma _loca _str _intPair1 _intPair2 _exp) =
    (sem_Exp_GenPragma _loca _str _intPair1 _intPair2 _exp)
sem_Exp (Proc _loca _pat _exp) =
    (sem_Exp_Proc _loca _pat _exp)
sem_Exp (LeftArrApp _loca _exp1 _exp2) =
    (sem_Exp_LeftArrApp _loca _exp1 _exp2)
sem_Exp (RightArrApp _loca _exp1 _exp2) =
    (sem_Exp_RightArrApp _loca _exp1 _exp2)
sem_Exp (LeftArrHighApp _loca _exp1 _exp2) =
    (sem_Exp_LeftArrHighApp _loca _exp1 _exp2)
sem_Exp (RightArrHighApp _loca _exp1 _exp2) =
    (sem_Exp_RightArrHighApp _loca _exp1 _exp2)
sem_Exp (LCase _loca _alts) =
    (sem_Exp_LCase _loca _alts)
-- semantic domain
type T_Exp l = ( )
data Inh_Exp l = Inh_Exp {}
data Syn_Exp l = Syn_Exp {}
wrap_Exp :: (T_Exp) (l) ->
            (Inh_Exp) (l) ->
            (Syn_Exp) (l)
wrap_Exp sem (Inh_Exp) =
    (let ( ) = sem
     in  (Syn_Exp))
sem_Exp_Var :: (l) ->
               (QName l) ->
               (T_Exp) (l)
sem_Exp_Var (loca_ :: (l)) (qName_ :: (QName l)) =
    (let
     in  ( ))
sem_Exp_OverloadedLabel :: (l) ->
                           (String) ->
                           (T_Exp) (l)
sem_Exp_OverloadedLabel (loca_ :: (l)) (label_ :: (String)) =
    (let
     in  ( ))
sem_Exp_IPVar :: (l) ->
                 (IPName l) ->
                 (T_Exp) (l)
sem_Exp_IPVar (loca_ :: (l)) (iPName_ :: (IPName l)) =
    (let
     in  ( ))
sem_Exp_Con :: (l) ->
               (QName l) ->
               (T_Exp) (l)
sem_Exp_Con (loca_ :: (l)) (qName_ :: (QName l)) =
    (let
     in  ( ))
sem_Exp_Lit :: (l) ->
               (Literal l) ->
               (T_Exp) (l)
sem_Exp_Lit (loca_ :: (l)) (literal_ :: (Literal l)) =
    (let
     in  ( ))
sem_Exp_InfixApp :: (l) ->
                    (Exp l) ->
                    (QOp l) ->
                    (Exp l) ->
                    (T_Exp) (l)
sem_Exp_InfixApp (loca_ :: (l)) (exp1_ :: (Exp l)) (qOp_ :: (QOp l)) (exp2_ :: (Exp l)) =
    (let
     in  ( ))
sem_Exp_App :: (l) ->
               (Exp l) ->
               (Exp l) ->
               (T_Exp) (l)
sem_Exp_App (loca_ :: (l)) (exp1_ :: (Exp l)) (exp2_ :: (Exp l)) =
    (let
     in  ( ))
sem_Exp_NegApp :: (l) ->
                  (Exp l) ->
                  (T_Exp) (l)
sem_Exp_NegApp (loca_ :: (l)) (exp_ :: (Exp l)) =
    (let
     in  ( ))
sem_Exp_Lambda :: (l) ->
                  ([Pat l]) ->
                  (Exp l) ->
                  (T_Exp) (l)
sem_Exp_Lambda (loca_ :: (l)) (pats_ :: ([Pat l])) (exp_ :: (Exp l)) =
    (let
     in  ( ))
sem_Exp_Let :: (l) ->
               (Binds l) ->
               (Exp l) ->
               (T_Exp) (l)
sem_Exp_Let (loca_ :: (l)) (binds_ :: (Binds l)) (exp_ :: (Exp l)) =
    (let
     in  ( ))
sem_Exp_If :: (l) ->
              (Exp l) ->
              (Exp l) ->
              (Exp l) ->
              (T_Exp) (l)
sem_Exp_If (loca_ :: (l)) (exp1_ :: (Exp l)) (exp2_ :: (Exp l)) (exp3_ :: (Exp l)) =
    (let
     in  ( ))
sem_Exp_MultiIf :: (l) ->
                   ([GuardedRhs l]) ->
                   (T_Exp) (l)
sem_Exp_MultiIf (loca_ :: (l)) (guardedRhss_ :: ([GuardedRhs l])) =
    (let
     in  ( ))
sem_Exp_Case :: (l) ->
                (Exp l) ->
                ([Alt l]) ->
                (T_Exp) (l)
sem_Exp_Case (loca_ :: (l)) (exp_ :: (Exp l)) (alts_ :: ([Alt l])) =
    (let
     in  ( ))
sem_Exp_Do :: (l) ->
              ([Stmt l]) ->
              (T_Exp) (l)
sem_Exp_Do (loca_ :: (l)) (stmts_ :: ([Stmt l])) =
    (let
     in  ( ))
sem_Exp_MDo :: (l) ->
               ([Stmt l]) ->
               (T_Exp) (l)
sem_Exp_MDo (loca_ :: (l)) (stmts_ :: ([Stmt l])) =
    (let
     in  ( ))
sem_Exp_Tuple :: (l) ->
                 (T_Boxed) ->
                 ([Exp l]) ->
                 (T_Exp) (l)
sem_Exp_Tuple (loca_ :: (l)) boxed_ (exps_ :: ([Exp l])) =
    (let
     in  ( ))
sem_Exp_UnboxedSum :: (l) ->
                      (Int) ->
                      (Int) ->
                      (Exp l) ->
                      (T_Exp) (l)
sem_Exp_UnboxedSum (loca_ :: (l)) (int1_ :: (Int)) (int2_ :: (Int)) (exp_ :: (Exp l)) =
    (let
     in  ( ))
sem_Exp_TupleSection :: (l) ->
                        (T_Boxed) ->
                        ([Maybe (Exp l)]) ->
                        (T_Exp) (l)
sem_Exp_TupleSection (loca_ :: (l)) boxed_ (mExps_ :: ([Maybe (Exp l)])) =
    (let
     in  ( ))
sem_Exp_List :: (l) ->
                ([Exp l]) ->
                (T_Exp) (l)
sem_Exp_List (loca_ :: (l)) (exps_ :: ([Exp l])) =
    (let
     in  ( ))
sem_Exp_ParArray :: (l) ->
                    ([Exp l]) ->
                    (T_Exp) (l)
sem_Exp_ParArray (loca_ :: (l)) (exps_ :: ([Exp l])) =
    (let
     in  ( ))
sem_Exp_Paren :: (l) ->
                 (Exp l) ->
                 (T_Exp) (l)
sem_Exp_Paren (loca_ :: (l)) (exp_ :: (Exp l)) =
    (let
     in  ( ))
sem_Exp_LeftSection :: (l) ->
                       (Exp l) ->
                       (QOp l) ->
                       (T_Exp) (l)
sem_Exp_LeftSection (loca_ :: (l)) (exp_ :: (Exp l)) (qOp_ :: (QOp l)) =
    (let
     in  ( ))
sem_Exp_RightSection :: (l) ->
                        (QOp l) ->
                        (Exp l) ->
                        (T_Exp) (l)
sem_Exp_RightSection (loca_ :: (l)) (qOp_ :: (QOp l)) (exp_ :: (Exp l)) =
    (let
     in  ( ))
sem_Exp_RecConstr :: (l) ->
                     (QName l) ->
                     ([FieldUpdate l]) ->
                     (T_Exp) (l)
sem_Exp_RecConstr (loca_ :: (l)) (qName_ :: (QName l)) (fieldUpdates_ :: ([FieldUpdate l])) =
    (let
     in  ( ))
sem_Exp_RecUpdate :: (l) ->
                     (Exp l) ->
                     ([FieldUpdate l]) ->
                     (T_Exp) (l)
sem_Exp_RecUpdate (loca_ :: (l)) (exp_ :: (Exp l)) (fieldUpdates_ :: ([FieldUpdate l])) =
    (let
     in  ( ))
sem_Exp_EnumFrom :: (l) ->
                    (Exp l) ->
                    (T_Exp) (l)
sem_Exp_EnumFrom (loca_ :: (l)) (exp_ :: (Exp l)) =
    (let
     in  ( ))
sem_Exp_EnumFromTo :: (l) ->
                      (Exp l) ->
                      (Exp l) ->
                      (T_Exp) (l)
sem_Exp_EnumFromTo (loca_ :: (l)) (exp1_ :: (Exp l)) (exp2_ :: (Exp l)) =
    (let
     in  ( ))
sem_Exp_EnumFromThen :: (l) ->
                        (Exp l) ->
                        (Exp l) ->
                        (T_Exp) (l)
sem_Exp_EnumFromThen (loca_ :: (l)) (exp1_ :: (Exp l)) (exp2_ :: (Exp l)) =
    (let
     in  ( ))
sem_Exp_EnumFromThenTo :: (l) ->
                          (Exp l) ->
                          (Exp l) ->
                          (Exp l) ->
                          (T_Exp) (l)
sem_Exp_EnumFromThenTo (loca_ :: (l)) (exp1_ :: (Exp l)) (exp2_ :: (Exp l)) (exp3_ :: (Exp l)) =
    (let
     in  ( ))
sem_Exp_ParArrayFromTo :: (l) ->
                          (Exp l) ->
                          (Exp l) ->
                          (T_Exp) (l)
sem_Exp_ParArrayFromTo (loca_ :: (l)) (exp1_ :: (Exp l)) (exp2_ :: (Exp l)) =
    (let
     in  ( ))
sem_Exp_ParArrayFromThenTo :: (l) ->
                              (Exp l) ->
                              (Exp l) ->
                              (Exp l) ->
                              (T_Exp) (l)
sem_Exp_ParArrayFromThenTo (loca_ :: (l)) (exp1_ :: (Exp l)) (exp2_ :: (Exp l)) (exp3_ :: (Exp l)) =
    (let
     in  ( ))
sem_Exp_ListComp :: (l) ->
                    (Exp l) ->
                    ([QualStmt l]) ->
                    (T_Exp) (l)
sem_Exp_ListComp (loca_ :: (l)) (exp_ :: (Exp l)) (qualStmts_ :: ([QualStmt l])) =
    (let
     in  ( ))
sem_Exp_ParComp :: (l) ->
                   (Exp l) ->
                   ([[QualStmt l]]) ->
                   (T_Exp) (l)
sem_Exp_ParComp (loca_ :: (l)) (exp_ :: (Exp l)) (qualStmtsList_ :: ([[QualStmt l]])) =
    (let
     in  ( ))
sem_Exp_ParArrayComp :: (l) ->
                        (Exp l) ->
                        ([[QualStmt l]]) ->
                        (T_Exp) (l)
sem_Exp_ParArrayComp (loca_ :: (l)) (exp_ :: (Exp l)) (qualStmtsList_ :: ([[QualStmt l]])) =
    (let
     in  ( ))
sem_Exp_ExpTypeSig :: (l) ->
                      (Exp l) ->
                      (Type l) ->
                      (T_Exp) (l)
sem_Exp_ExpTypeSig (loca_ :: (l)) (exp_ :: (Exp l)) (typ_ :: (Type l)) =
    (let
     in  ( ))
sem_Exp_VarQuote :: (l) ->
                    (QName l) ->
                    (T_Exp) (l)
sem_Exp_VarQuote (loca_ :: (l)) (qName_ :: (QName l)) =
    (let
     in  ( ))
sem_Exp_TypQuote :: (l) ->
                    (QName l) ->
                    (T_Exp) (l)
sem_Exp_TypQuote (loca_ :: (l)) (qName_ :: (QName l)) =
    (let
     in  ( ))
sem_Exp_BracketExp :: (l) ->
                      (Bracket l) ->
                      (T_Exp) (l)
sem_Exp_BracketExp (loca_ :: (l)) (bracket_ :: (Bracket l)) =
    (let
     in  ( ))
sem_Exp_SpliceExp :: (l) ->
                     (Splice l) ->
                     (T_Exp) (l)
sem_Exp_SpliceExp (loca_ :: (l)) (splice_ :: (Splice l)) =
    (let
     in  ( ))
sem_Exp_QuasiQuote :: (l) ->
                      (String) ->
                      (String) ->
                      (T_Exp) (l)
sem_Exp_QuasiQuote (loca_ :: (l)) (qqName_ :: (String)) (qqString_ :: (String)) =
    (let
     in  ( ))
sem_Exp_TypeApp :: (l) ->
                   (Type l) ->
                   (T_Exp) (l)
sem_Exp_TypeApp (loca_ :: (l)) (typ_ :: (Type l)) =
    (let
     in  ( ))
sem_Exp_XTag :: (l) ->
                (XName l) ->
                ([XAttr l]) ->
                (Maybe (Exp l)) ->
                ([Exp l]) ->
                (T_Exp) (l)
sem_Exp_XTag (loca_ :: (l)) (xName_ :: (XName l)) (xAttrs_ :: ([XAttr l])) (mExp_ :: (Maybe (Exp l))) (exps_ :: ([Exp l])) =
    (let
     in  ( ))
sem_Exp_XETag :: (l) ->
                 (XName l) ->
                 ([XAttr l]) ->
                 (Maybe (Exp l)) ->
                 (T_Exp) (l)
sem_Exp_XETag (loca_ :: (l)) (xName_ :: (XName l)) (xAttrs_ :: ([XAttr l])) (mExp_ :: (Maybe (Exp l))) =
    (let
     in  ( ))
sem_Exp_XPcdata :: (l) ->
                   (String) ->
                   (T_Exp) (l)
sem_Exp_XPcdata (loca_ :: (l)) (pcData_ :: (String)) =
    (let
     in  ( ))
sem_Exp_XExpTag :: (l) ->
                   (Exp l) ->
                   (T_Exp) (l)
sem_Exp_XExpTag (loca_ :: (l)) (exp_ :: (Exp l)) =
    (let
     in  ( ))
sem_Exp_XChildTag :: (l) ->
                     ([Exp l]) ->
                     (T_Exp) (l)
sem_Exp_XChildTag (loca_ :: (l)) (exps_ :: ([Exp l])) =
    (let
     in  ( ))
sem_Exp_CorePragma :: (l) ->
                      (String) ->
                      (Exp l) ->
                      (T_Exp) (l)
sem_Exp_CorePragma (loca_ :: (l)) (str_ :: (String)) (exp_ :: (Exp l)) =
    (let
     in  ( ))
sem_Exp_SCCPragma :: (l) ->
                     (String) ->
                     (Exp l) ->
                     (T_Exp) (l)
sem_Exp_SCCPragma (loca_ :: (l)) (str_ :: (String)) (exp_ :: (Exp l)) =
    (let
     in  ( ))
sem_Exp_GenPragma :: (l) ->
                     (String) ->
                     ((Int, Int)) ->
                     ((Int, Int)) ->
                     (Exp l) ->
                     (T_Exp) (l)
sem_Exp_GenPragma (loca_ :: (l)) (str_ :: (String)) (intPair1_ :: ((Int, Int))) (intPair2_ :: ((Int, Int))) (exp_ :: (Exp l)) =
    (let
     in  ( ))
sem_Exp_Proc :: (l) ->
                (Pat l) ->
                (Exp l) ->
                (T_Exp) (l)
sem_Exp_Proc (loca_ :: (l)) (pat_ :: (Pat l)) (exp_ :: (Exp l)) =
    (let
     in  ( ))
sem_Exp_LeftArrApp :: (l) ->
                      (Exp l) ->
                      (Exp l) ->
                      (T_Exp) (l)
sem_Exp_LeftArrApp (loca_ :: (l)) (exp1_ :: (Exp l)) (exp2_ :: (Exp l)) =
    (let
     in  ( ))
sem_Exp_RightArrApp :: (l) ->
                       (Exp l) ->
                       (Exp l) ->
                       (T_Exp) (l)
sem_Exp_RightArrApp (loca_ :: (l)) (exp1_ :: (Exp l)) (exp2_ :: (Exp l)) =
    (let
     in  ( ))
sem_Exp_LeftArrHighApp :: (l) ->
                          (Exp l) ->
                          (Exp l) ->
                          (T_Exp) (l)
sem_Exp_LeftArrHighApp (loca_ :: (l)) (exp1_ :: (Exp l)) (exp2_ :: (Exp l)) =
    (let
     in  ( ))
sem_Exp_RightArrHighApp :: (l) ->
                           (Exp l) ->
                           (Exp l) ->
                           (T_Exp) (l)
sem_Exp_RightArrHighApp (loca_ :: (l)) (exp1_ :: (Exp l)) (exp2_ :: (Exp l)) =
    (let
     in  ( ))
sem_Exp_LCase :: (l) ->
                 ([Alt l]) ->
                 (T_Exp) (l)
sem_Exp_LCase (loca_ :: (l)) (alts_ :: ([Alt l])) =
    (let
     in  ( ))
-- ExportSpec --------------------------------------------------
-- cata
sem_ExportSpec :: (ExportSpec) (l) ->
                  (T_ExportSpec) (l)
sem_ExportSpec (EVar _loca _qName) =
    (sem_ExportSpec_EVar _loca _qName)
sem_ExportSpec (EAbs _loca _namespace _qName) =
    (sem_ExportSpec_EAbs _loca _namespace _qName)
sem_ExportSpec (EThingWith _loca _eWildcard _qName _cNames) =
    (sem_ExportSpec_EThingWith _loca _eWildcard _qName _cNames)
sem_ExportSpec (EModuleContents _loca _moduleName) =
    (sem_ExportSpec_EModuleContents _loca _moduleName)
-- semantic domain
type T_ExportSpec l = ( )
data Inh_ExportSpec l = Inh_ExportSpec {}
data Syn_ExportSpec l = Syn_ExportSpec {}
wrap_ExportSpec :: (T_ExportSpec) (l) ->
                   (Inh_ExportSpec) (l) ->
                   (Syn_ExportSpec) (l)
wrap_ExportSpec sem (Inh_ExportSpec) =
    (let ( ) = sem
     in  (Syn_ExportSpec))
sem_ExportSpec_EVar :: (l) ->
                       (QName l) ->
                       (T_ExportSpec) (l)
sem_ExportSpec_EVar (loca_ :: (l)) (qName_ :: (QName l)) =
    (let
     in  ( ))
sem_ExportSpec_EAbs :: (l) ->
                       (Namespace l) ->
                       (QName l) ->
                       (T_ExportSpec) (l)
sem_ExportSpec_EAbs (loca_ :: (l)) (namespace_ :: (Namespace l)) (qName_ :: (QName l)) =
    (let
     in  ( ))
sem_ExportSpec_EThingWith :: (l) ->
                             (EWildcard l) ->
                             (QName l) ->
                             ([CName l]) ->
                             (T_ExportSpec) (l)
sem_ExportSpec_EThingWith (loca_ :: (l)) (eWildcard_ :: (EWildcard l)) (qName_ :: (QName l)) (cNames_ :: ([CName l])) =
    (let
     in  ( ))
sem_ExportSpec_EModuleContents :: (l) ->
                                  (ModuleName l) ->
                                  (T_ExportSpec) (l)
sem_ExportSpec_EModuleContents (loca_ :: (l)) (moduleName_ :: (ModuleName l)) =
    (let
     in  ( ))
-- ExportSpecList ----------------------------------------------
-- cata
sem_ExportSpecList :: (ExportSpecList) (l) ->
                      (T_ExportSpecList) (l)
sem_ExportSpecList (ExportSpecList _loca _exportSpec) =
    (sem_ExportSpecList_ExportSpecList _loca _exportSpec)
-- semantic domain
type T_ExportSpecList l = ( )
data Inh_ExportSpecList l = Inh_ExportSpecList {}
data Syn_ExportSpecList l = Syn_ExportSpecList {}
wrap_ExportSpecList :: (T_ExportSpecList) (l) ->
                       (Inh_ExportSpecList) (l) ->
                       (Syn_ExportSpecList) (l)
wrap_ExportSpecList sem (Inh_ExportSpecList) =
    (let ( ) = sem
     in  (Syn_ExportSpecList))
sem_ExportSpecList_ExportSpecList :: (l) ->
                                     ([ExportSpec l]) ->
                                     (T_ExportSpecList) (l)
sem_ExportSpecList_ExportSpecList (loca_ :: (l)) (exportSpec_ :: ([ExportSpec l])) =
    (let
     in  ( ))
-- FieldDecl ---------------------------------------------------
-- cata
sem_FieldDecl :: (FieldDecl) (l) ->
                 (T_FieldDecl) (l)
sem_FieldDecl (FieldDecl _loca _names _typ) =
    (sem_FieldDecl_FieldDecl _loca _names _typ)
-- semantic domain
type T_FieldDecl l = ( )
data Inh_FieldDecl l = Inh_FieldDecl {}
data Syn_FieldDecl l = Syn_FieldDecl {}
wrap_FieldDecl :: (T_FieldDecl) (l) ->
                  (Inh_FieldDecl) (l) ->
                  (Syn_FieldDecl) (l)
wrap_FieldDecl sem (Inh_FieldDecl) =
    (let ( ) = sem
     in  (Syn_FieldDecl))
sem_FieldDecl_FieldDecl :: (l) ->
                           ([Name l]) ->
                           (Type l) ->
                           (T_FieldDecl) (l)
sem_FieldDecl_FieldDecl (loca_ :: (l)) (names_ :: ([Name l])) (typ_ :: (Type l)) =
    (let
     in  ( ))
-- FieldUpdate -------------------------------------------------
-- cata
sem_FieldUpdate :: (FieldUpdate) (l) ->
                   (T_FieldUpdate) (l)
sem_FieldUpdate (FieldUpdate _loca _qName _exp) =
    (sem_FieldUpdate_FieldUpdate _loca _qName _exp)
sem_FieldUpdate (FieldPun _loca _qName) =
    (sem_FieldUpdate_FieldPun _loca _qName)
sem_FieldUpdate (FieldWildcard _loca) =
    (sem_FieldUpdate_FieldWildcard _loca)
-- semantic domain
type T_FieldUpdate l = ( )
data Inh_FieldUpdate l = Inh_FieldUpdate {}
data Syn_FieldUpdate l = Syn_FieldUpdate {}
wrap_FieldUpdate :: (T_FieldUpdate) (l) ->
                    (Inh_FieldUpdate) (l) ->
                    (Syn_FieldUpdate) (l)
wrap_FieldUpdate sem (Inh_FieldUpdate) =
    (let ( ) = sem
     in  (Syn_FieldUpdate))
sem_FieldUpdate_FieldUpdate :: (l) ->
                               (QName l) ->
                               (Exp l) ->
                               (T_FieldUpdate) (l)
sem_FieldUpdate_FieldUpdate (loca_ :: (l)) (qName_ :: (QName l)) (exp_ :: (Exp l)) =
    (let
     in  ( ))
sem_FieldUpdate_FieldPun :: (l) ->
                            (QName l) ->
                            (T_FieldUpdate) (l)
sem_FieldUpdate_FieldPun (loca_ :: (l)) (qName_ :: (QName l)) =
    (let
     in  ( ))
sem_FieldUpdate_FieldWildcard :: (l) ->
                                 (T_FieldUpdate) (l)
sem_FieldUpdate_FieldWildcard (loca_ :: (l)) =
    (let
     in  ( ))
-- FunDep ------------------------------------------------------
-- cata
sem_FunDep :: (FunDep) (l) ->
              (T_FunDep) (l)
sem_FunDep (FunDep _loca _leftNames _rightNames) =
    (sem_FunDep_FunDep _loca _leftNames _rightNames)
-- semantic domain
type T_FunDep l = ( )
data Inh_FunDep l = Inh_FunDep {}
data Syn_FunDep l = Syn_FunDep {}
wrap_FunDep :: (T_FunDep) (l) ->
               (Inh_FunDep) (l) ->
               (Syn_FunDep) (l)
wrap_FunDep sem (Inh_FunDep) =
    (let ( ) = sem
     in  (Syn_FunDep))
sem_FunDep_FunDep :: (l) ->
                     ([Name l]) ->
                     ([Name l]) ->
                     (T_FunDep) (l)
sem_FunDep_FunDep (loca_ :: (l)) (leftNames_ :: ([Name l])) (rightNames_ :: ([Name l])) =
    (let
     in  ( ))
-- GadtDecl ----------------------------------------------------
-- cata
sem_GadtDecl :: (GadtDecl) (l) ->
                (T_GadtDecl) (l)
sem_GadtDecl (GadtDecl _loca _name _mFieldDecls _typ) =
    (sem_GadtDecl_GadtDecl _loca _name _mFieldDecls _typ)
-- semantic domain
type T_GadtDecl l = ( )
data Inh_GadtDecl l = Inh_GadtDecl {}
data Syn_GadtDecl l = Syn_GadtDecl {}
wrap_GadtDecl :: (T_GadtDecl) (l) ->
                 (Inh_GadtDecl) (l) ->
                 (Syn_GadtDecl) (l)
wrap_GadtDecl sem (Inh_GadtDecl) =
    (let ( ) = sem
     in  (Syn_GadtDecl))
sem_GadtDecl_GadtDecl :: (l) ->
                         (Name l) ->
                         (Maybe [FieldDecl l]) ->
                         (Type l) ->
                         (T_GadtDecl) (l)
sem_GadtDecl_GadtDecl (loca_ :: (l)) (name_ :: (Name l)) (mFieldDecls_ :: (Maybe [FieldDecl l])) (typ_ :: (Type l)) =
    (let
     in  ( ))
-- GuardedRhs --------------------------------------------------
-- cata
sem_GuardedRhs :: (GuardedRhs) (l) ->
                  (T_GuardedRhs) (l)
sem_GuardedRhs (GuardedRhs _loca _stmts _exp) =
    (sem_GuardedRhs_GuardedRhs _loca _stmts _exp)
-- semantic domain
type T_GuardedRhs l = ( )
data Inh_GuardedRhs l = Inh_GuardedRhs {}
data Syn_GuardedRhs l = Syn_GuardedRhs {}
wrap_GuardedRhs :: (T_GuardedRhs) (l) ->
                   (Inh_GuardedRhs) (l) ->
                   (Syn_GuardedRhs) (l)
wrap_GuardedRhs sem (Inh_GuardedRhs) =
    (let ( ) = sem
     in  (Syn_GuardedRhs))
sem_GuardedRhs_GuardedRhs :: (l) ->
                             ([Stmt l]) ->
                             (Exp l) ->
                             (T_GuardedRhs) (l)
sem_GuardedRhs_GuardedRhs (loca_ :: (l)) (stmts_ :: ([Stmt l])) (exp_ :: (Exp l)) =
    (let
     in  ( ))
-- IPBind ------------------------------------------------------
-- cata
sem_IPBind :: (IPBind) (l) ->
              (T_IPBind) (l)
sem_IPBind (IPBind _loca _iPName _exp) =
    (sem_IPBind_IPBind _loca _iPName _exp)
-- semantic domain
type T_IPBind l = ( )
data Inh_IPBind l = Inh_IPBind {}
data Syn_IPBind l = Syn_IPBind {}
wrap_IPBind :: (T_IPBind) (l) ->
               (Inh_IPBind) (l) ->
               (Syn_IPBind) (l)
wrap_IPBind sem (Inh_IPBind) =
    (let ( ) = sem
     in  (Syn_IPBind))
sem_IPBind_IPBind :: (l) ->
                     (IPName l) ->
                     (Exp l) ->
                     (T_IPBind) (l)
sem_IPBind_IPBind (loca_ :: (l)) (iPName_ :: (IPName l)) (exp_ :: (Exp l)) =
    (let
     in  ( ))
-- IPName ------------------------------------------------------
-- cata
sem_IPName :: (IPName) (l) ->
              (T_IPName) (l)
sem_IPName (IPDup _loca _ipDup) =
    (sem_IPName_IPDup _loca _ipDup)
sem_IPName (IPLin _loca _ipLin) =
    (sem_IPName_IPLin _loca _ipLin)
-- semantic domain
type T_IPName l = ( )
data Inh_IPName l = Inh_IPName {}
data Syn_IPName l = Syn_IPName {}
wrap_IPName :: (T_IPName) (l) ->
               (Inh_IPName) (l) ->
               (Syn_IPName) (l)
wrap_IPName sem (Inh_IPName) =
    (let ( ) = sem
     in  (Syn_IPName))
sem_IPName_IPDup :: (l) ->
                    (String) ->
                    (T_IPName) (l)
sem_IPName_IPDup (loca_ :: (l)) (ipDup_ :: (String)) =
    (let
     in  ( ))
sem_IPName_IPLin :: (l) ->
                    (String) ->
                    (T_IPName) (l)
sem_IPName_IPLin (loca_ :: (l)) (ipLin_ :: (String)) =
    (let
     in  ( ))
-- ImportDecl --------------------------------------------------
-- cata
sem_ImportDecl :: (ImportDecl) (l) ->
                  (T_ImportDecl) (l)
sem_ImportDecl (ImportDecl _importAnn _importModule _importQualified _importSrc _importSafe _importPkg _importAs _importSpecs) =
    (sem_ImportDecl_ImportDecl _importAnn _importModule _importQualified _importSrc _importSafe _importPkg _importAs _importSpecs)
-- semantic domain
type T_ImportDecl l = ( )
data Inh_ImportDecl l = Inh_ImportDecl {}
data Syn_ImportDecl l = Syn_ImportDecl {}
wrap_ImportDecl :: (T_ImportDecl) (l) ->
                   (Inh_ImportDecl) (l) ->
                   (Syn_ImportDecl) (l)
wrap_ImportDecl sem (Inh_ImportDecl) =
    (let ( ) = sem
     in  (Syn_ImportDecl))
sem_ImportDecl_ImportDecl :: (l) ->
                             (ModuleName l) ->
                             (Bool) ->
                             (Bool) ->
                             (Bool) ->
                             (Maybe String) ->
                             (Maybe (ModuleName l)) ->
                             (Maybe (ImportSpecList l)) ->
                             (T_ImportDecl) (l)
sem_ImportDecl_ImportDecl (importAnn_ :: (l)) (importModule_ :: (ModuleName l)) (importQualified_ :: (Bool)) (importSrc_ :: (Bool)) (importSafe_ :: (Bool)) (importPkg_ :: (Maybe String)) (importAs_ :: (Maybe (ModuleName l))) (importSpecs_ :: (Maybe (ImportSpecList l))) =
    (let
     in  ( ))
-- ImportSpec --------------------------------------------------
-- cata
sem_ImportSpec :: (ImportSpec) (l) ->
                  (T_ImportSpec) (l)
sem_ImportSpec (IVar _loca _variableName) =
    (sem_ImportSpec_IVar _loca _variableName)
sem_ImportSpec (IAbs _loca _namespace _name) =
    (sem_ImportSpec_IAbs _loca (sem_Namespace _namespace) _name)
sem_ImportSpec (IThingAll _loca _name) =
    (sem_ImportSpec_IThingAll _loca _name)
sem_ImportSpec (IThingWith _loca _name _cNames) =
    (sem_ImportSpec_IThingWith _loca _name _cNames)
-- semantic domain
type T_ImportSpec l = ( )
data Inh_ImportSpec l = Inh_ImportSpec {}
data Syn_ImportSpec l = Syn_ImportSpec {}
wrap_ImportSpec :: (T_ImportSpec) (l) ->
                   (Inh_ImportSpec) (l) ->
                   (Syn_ImportSpec) (l)
wrap_ImportSpec sem (Inh_ImportSpec) =
    (let ( ) = sem
     in  (Syn_ImportSpec))
sem_ImportSpec_IVar :: (l) ->
                       (Name l) ->
                       (T_ImportSpec) (l)
sem_ImportSpec_IVar (loca_ :: (l)) (variableName_ :: (Name l)) =
    (let
     in  ( ))
sem_ImportSpec_IAbs :: (l) ->
                       (T_Namespace) (l) ->
                       (Name l) ->
                       (T_ImportSpec) (l)
sem_ImportSpec_IAbs (loca_ :: (l)) (namespace_ :: ( )) (name_ :: (Name l)) =
    (let
     in  ( ))
sem_ImportSpec_IThingAll :: (l) ->
                            (Name l) ->
                            (T_ImportSpec) (l)
sem_ImportSpec_IThingAll (loca_ :: (l)) (name_ :: (Name l)) =
    (let
     in  ( ))
sem_ImportSpec_IThingWith :: (l) ->
                             (Name l) ->
                             ([CName l]) ->
                             (T_ImportSpec) (l)
sem_ImportSpec_IThingWith (loca_ :: (l)) (name_ :: (Name l)) (cNames_ :: ([CName l])) =
    (let
     in  ( ))
-- ImportSpecList ----------------------------------------------
-- cata
sem_ImportSpecList :: (ImportSpecList) (l) ->
                      (T_ImportSpecList) (l)
sem_ImportSpecList (ImportSpecList _loca _hiding _importSpecs) =
    (sem_ImportSpecList_ImportSpecList _loca _hiding _importSpecs)
-- semantic domain
type T_ImportSpecList l = ( )
data Inh_ImportSpecList l = Inh_ImportSpecList {}
data Syn_ImportSpecList l = Syn_ImportSpecList {}
wrap_ImportSpecList :: (T_ImportSpecList) (l) ->
                       (Inh_ImportSpecList) (l) ->
                       (Syn_ImportSpecList) (l)
wrap_ImportSpecList sem (Inh_ImportSpecList) =
    (let ( ) = sem
     in  (Syn_ImportSpecList))
sem_ImportSpecList_ImportSpecList :: (l) ->
                                     (Bool) ->
                                     ([ImportSpec l]) ->
                                     (T_ImportSpecList) (l)
sem_ImportSpecList_ImportSpecList (loca_ :: (l)) (hiding_ :: (Bool)) (importSpecs_ :: ([ImportSpec l])) =
    (let
     in  ( ))
-- InjectivityInfo ---------------------------------------------
-- cata
sem_InjectivityInfo :: (InjectivityInfo) (l) ->
                       (T_InjectivityInfo) (l)
sem_InjectivityInfo (InjectivityInfo _loca _name _names) =
    (sem_InjectivityInfo_InjectivityInfo _loca _name _names)
-- semantic domain
type T_InjectivityInfo l = ( )
data Inh_InjectivityInfo l = Inh_InjectivityInfo {}
data Syn_InjectivityInfo l = Syn_InjectivityInfo {}
wrap_InjectivityInfo :: (T_InjectivityInfo) (l) ->
                        (Inh_InjectivityInfo) (l) ->
                        (Syn_InjectivityInfo) (l)
wrap_InjectivityInfo sem (Inh_InjectivityInfo) =
    (let ( ) = sem
     in  (Syn_InjectivityInfo))
sem_InjectivityInfo_InjectivityInfo :: (l) ->
                                       (Name l) ->
                                       ([Name l]) ->
                                       (T_InjectivityInfo) (l)
sem_InjectivityInfo_InjectivityInfo (loca_ :: (l)) (name_ :: (Name l)) (names_ :: ([Name l])) =
    (let
     in  ( ))
-- InstDecl ----------------------------------------------------
-- cata
sem_InstDecl :: (InstDecl) (l) ->
                (T_InstDecl) (l)
sem_InstDecl (InsDecl _loca _decl) =
    (sem_InstDecl_InsDecl _loca _decl)
sem_InstDecl (InsType _loca _typ1 _typ2) =
    (sem_InstDecl_InsType _loca _typ1 _typ2)
sem_InstDecl (InsData _loca _dataOrNew _typ _qualConDecl _derivings) =
    (sem_InstDecl_InsData _loca _dataOrNew _typ _qualConDecl _derivings)
sem_InstDecl (InsGData _loca _dataOrNew _typ _mKind _gadtDecls _derivings) =
    (sem_InstDecl_InsGData _loca _dataOrNew _typ _mKind _gadtDecls _derivings)
-- semantic domain
type T_InstDecl l = ( )
data Inh_InstDecl l = Inh_InstDecl {}
data Syn_InstDecl l = Syn_InstDecl {}
wrap_InstDecl :: (T_InstDecl) (l) ->
                 (Inh_InstDecl) (l) ->
                 (Syn_InstDecl) (l)
wrap_InstDecl sem (Inh_InstDecl) =
    (let ( ) = sem
     in  (Syn_InstDecl))
sem_InstDecl_InsDecl :: (l) ->
                        (Decl l) ->
                        (T_InstDecl) (l)
sem_InstDecl_InsDecl (loca_ :: (l)) (decl_ :: (Decl l)) =
    (let
     in  ( ))
sem_InstDecl_InsType :: (l) ->
                        (Type l) ->
                        (Type l) ->
                        (T_InstDecl) (l)
sem_InstDecl_InsType (loca_ :: (l)) (typ1_ :: (Type l)) (typ2_ :: (Type l)) =
    (let
     in  ( ))
sem_InstDecl_InsData :: (l) ->
                        (DataOrNew l) ->
                        (Type l) ->
                        ([QualConDecl l]) ->
                        ([Deriving l]) ->
                        (T_InstDecl) (l)
sem_InstDecl_InsData (loca_ :: (l)) (dataOrNew_ :: (DataOrNew l)) (typ_ :: (Type l)) (qualConDecl_ :: ([QualConDecl l])) (derivings_ :: ([Deriving l])) =
    (let
     in  ( ))
sem_InstDecl_InsGData :: (l) ->
                         (DataOrNew l) ->
                         (Type l) ->
                         (Maybe (Kind l)) ->
                         ([GadtDecl l]) ->
                         ([Deriving l]) ->
                         (T_InstDecl) (l)
sem_InstDecl_InsGData (loca_ :: (l)) (dataOrNew_ :: (DataOrNew l)) (typ_ :: (Type l)) (mKind_ :: (Maybe (Kind l))) (gadtDecls_ :: ([GadtDecl l])) (derivings_ :: ([Deriving l])) =
    (let
     in  ( ))
-- InstHead ----------------------------------------------------
-- cata
sem_InstHead :: (InstHead) (l) ->
                (T_InstHead) (l)
sem_InstHead (IHCon _loca _qName) =
    (sem_InstHead_IHCon _loca _qName)
sem_InstHead (IHInfix _loca _typ _qName) =
    (sem_InstHead_IHInfix _loca _typ _qName)
sem_InstHead (IHParen _loca _instHead) =
    (sem_InstHead_IHParen _loca _instHead)
sem_InstHead (IHApp _loca _instHead _typ) =
    (sem_InstHead_IHApp _loca _instHead _typ)
-- semantic domain
type T_InstHead l = ( )
data Inh_InstHead l = Inh_InstHead {}
data Syn_InstHead l = Syn_InstHead {}
wrap_InstHead :: (T_InstHead) (l) ->
                 (Inh_InstHead) (l) ->
                 (Syn_InstHead) (l)
wrap_InstHead sem (Inh_InstHead) =
    (let ( ) = sem
     in  (Syn_InstHead))
sem_InstHead_IHCon :: (l) ->
                      (QName l) ->
                      (T_InstHead) (l)
sem_InstHead_IHCon (loca_ :: (l)) (qName_ :: (QName l)) =
    (let
     in  ( ))
sem_InstHead_IHInfix :: (l) ->
                        (Type l) ->
                        (QName l) ->
                        (T_InstHead) (l)
sem_InstHead_IHInfix (loca_ :: (l)) (typ_ :: (Type l)) (qName_ :: (QName l)) =
    (let
     in  ( ))
sem_InstHead_IHParen :: (l) ->
                        (InstHead l) ->
                        (T_InstHead) (l)
sem_InstHead_IHParen (loca_ :: (l)) (instHead_ :: (InstHead l)) =
    (let
     in  ( ))
sem_InstHead_IHApp :: (l) ->
                      (InstHead l) ->
                      (Type l) ->
                      (T_InstHead) (l)
sem_InstHead_IHApp (loca_ :: (l)) (instHead_ :: (InstHead l)) (typ_ :: (Type l)) =
    (let
     in  ( ))
-- InstRule ----------------------------------------------------
-- cata
sem_InstRule :: (InstRule) (l) ->
                (T_InstRule) (l)
sem_InstRule (IRule _loca _mTyVarBinds _mContext _instHead) =
    (sem_InstRule_IRule _loca _mTyVarBinds _mContext _instHead)
sem_InstRule (IParen _loca _instRule) =
    (sem_InstRule_IParen _loca _instRule)
-- semantic domain
type T_InstRule l = ( )
data Inh_InstRule l = Inh_InstRule {}
data Syn_InstRule l = Syn_InstRule {}
wrap_InstRule :: (T_InstRule) (l) ->
                 (Inh_InstRule) (l) ->
                 (Syn_InstRule) (l)
wrap_InstRule sem (Inh_InstRule) =
    (let ( ) = sem
     in  (Syn_InstRule))
sem_InstRule_IRule :: (l) ->
                      (Maybe [TyVarBind l]) ->
                      (Maybe (Context l)) ->
                      (InstHead l) ->
                      (T_InstRule) (l)
sem_InstRule_IRule (loca_ :: (l)) (mTyVarBinds_ :: (Maybe [TyVarBind l])) (mContext_ :: (Maybe (Context l))) (instHead_ :: (InstHead l)) =
    (let
     in  ( ))
sem_InstRule_IParen :: (l) ->
                       (InstRule l) ->
                       (T_InstRule) (l)
sem_InstRule_IParen (loca_ :: (l)) (instRule_ :: (InstRule l)) =
    (let
     in  ( ))
-- Kind --------------------------------------------------------
-- cata
sem_Kind :: (Kind) (l) ->
            (T_Kind) (l)
sem_Kind (KindStar _loca) =
    (sem_Kind_KindStar _loca)
sem_Kind (KindFn _loca _kind1 _kind2) =
    (sem_Kind_KindFn _loca _kind1 _kind2)
sem_Kind (KindParen _loca _kind) =
    (sem_Kind_KindParen _loca _kind)
sem_Kind (KindVar _loca _qName) =
    (sem_Kind_KindVar _loca _qName)
sem_Kind (KindApp _loca _kind1 _kind2) =
    (sem_Kind_KindApp _loca _kind1 _kind2)
sem_Kind (KindTuple _loca _kinds) =
    (sem_Kind_KindTuple _loca _kinds)
sem_Kind (KindList _loca _kind) =
    (sem_Kind_KindList _loca _kind)
-- semantic domain
type T_Kind l = ( )
data Inh_Kind l = Inh_Kind {}
data Syn_Kind l = Syn_Kind {}
wrap_Kind :: (T_Kind) (l) ->
             (Inh_Kind) (l) ->
             (Syn_Kind) (l)
wrap_Kind sem (Inh_Kind) =
    (let ( ) = sem
     in  (Syn_Kind))
sem_Kind_KindStar :: (l) ->
                     (T_Kind) (l)
sem_Kind_KindStar (loca_ :: (l)) =
    (let
     in  ( ))
sem_Kind_KindFn :: (l) ->
                   (Kind l) ->
                   (Kind l) ->
                   (T_Kind) (l)
sem_Kind_KindFn (loca_ :: (l)) (kind1_ :: (Kind l)) (kind2_ :: (Kind l)) =
    (let
     in  ( ))
sem_Kind_KindParen :: (l) ->
                      (Kind l) ->
                      (T_Kind) (l)
sem_Kind_KindParen (loca_ :: (l)) (kind_ :: (Kind l)) =
    (let
     in  ( ))
sem_Kind_KindVar :: (l) ->
                    (QName l) ->
                    (T_Kind) (l)
sem_Kind_KindVar (loca_ :: (l)) (qName_ :: (QName l)) =
    (let
     in  ( ))
sem_Kind_KindApp :: (l) ->
                    (Kind l) ->
                    (Kind l) ->
                    (T_Kind) (l)
sem_Kind_KindApp (loca_ :: (l)) (kind1_ :: (Kind l)) (kind2_ :: (Kind l)) =
    (let
     in  ( ))
sem_Kind_KindTuple :: (l) ->
                      ([Kind l]) ->
                      (T_Kind) (l)
sem_Kind_KindTuple (loca_ :: (l)) (kinds_ :: ([Kind l])) =
    (let
     in  ( ))
sem_Kind_KindList :: (l) ->
                     (Kind l) ->
                     (T_Kind) (l)
sem_Kind_KindList (loca_ :: (l)) (kind_ :: (Kind l)) =
    (let
     in  ( ))
-- Literal -----------------------------------------------------
-- cata
sem_Literal :: (Literal) (l) ->
               (T_Literal) (l)
sem_Literal (Char _loca _lit _litStr) =
    (sem_Literal_Char _loca _lit _litStr)
sem_Literal (String _loca _lit _litStr) =
    (sem_Literal_String _loca _lit _litStr)
sem_Literal (Int _loca _lit _litStr) =
    (sem_Literal_Int _loca _lit _litStr)
sem_Literal (Frac _loca _lit _litStr) =
    (sem_Literal_Frac _loca _lit _litStr)
sem_Literal (PrimInt _loca _lit _litStr) =
    (sem_Literal_PrimInt _loca _lit _litStr)
sem_Literal (PrimWord _loca _lit _litStr) =
    (sem_Literal_PrimWord _loca _lit _litStr)
sem_Literal (PrimFloat _loca _lit _litStr) =
    (sem_Literal_PrimFloat _loca _lit _litStr)
sem_Literal (PrimDouble _loca _lit _litStr) =
    (sem_Literal_PrimDouble _loca _lit _litStr)
sem_Literal (PrimChar _loca _lit _litStr) =
    (sem_Literal_PrimChar _loca _lit _litStr)
sem_Literal (PrimString _loca _lit _litStr) =
    (sem_Literal_PrimString _loca _lit _litStr)
-- semantic domain
type T_Literal l = ( )
data Inh_Literal l = Inh_Literal {}
data Syn_Literal l = Syn_Literal {}
wrap_Literal :: (T_Literal) (l) ->
                (Inh_Literal) (l) ->
                (Syn_Literal) (l)
wrap_Literal sem (Inh_Literal) =
    (let ( ) = sem
     in  (Syn_Literal))
sem_Literal_Char :: (l) ->
                    (Char) ->
                    (String) ->
                    (T_Literal) (l)
sem_Literal_Char (loca_ :: (l)) (lit_ :: (Char)) (litStr_ :: (String)) =
    (let
     in  ( ))
sem_Literal_String :: (l) ->
                      (String) ->
                      (String) ->
                      (T_Literal) (l)
sem_Literal_String (loca_ :: (l)) (lit_ :: (String)) (litStr_ :: (String)) =
    (let
     in  ( ))
sem_Literal_Int :: (l) ->
                   (Integer) ->
                   (String) ->
                   (T_Literal) (l)
sem_Literal_Int (loca_ :: (l)) (lit_ :: (Integer)) (litStr_ :: (String)) =
    (let
     in  ( ))
sem_Literal_Frac :: (l) ->
                    (Rational) ->
                    (String) ->
                    (T_Literal) (l)
sem_Literal_Frac (loca_ :: (l)) (lit_ :: (Rational)) (litStr_ :: (String)) =
    (let
     in  ( ))
sem_Literal_PrimInt :: (l) ->
                       (Integer) ->
                       (String) ->
                       (T_Literal) (l)
sem_Literal_PrimInt (loca_ :: (l)) (lit_ :: (Integer)) (litStr_ :: (String)) =
    (let
     in  ( ))
sem_Literal_PrimWord :: (l) ->
                        (Integer) ->
                        (String) ->
                        (T_Literal) (l)
sem_Literal_PrimWord (loca_ :: (l)) (lit_ :: (Integer)) (litStr_ :: (String)) =
    (let
     in  ( ))
sem_Literal_PrimFloat :: (l) ->
                         (Rational) ->
                         (String) ->
                         (T_Literal) (l)
sem_Literal_PrimFloat (loca_ :: (l)) (lit_ :: (Rational)) (litStr_ :: (String)) =
    (let
     in  ( ))
sem_Literal_PrimDouble :: (l) ->
                          (Rational) ->
                          (String) ->
                          (T_Literal) (l)
sem_Literal_PrimDouble (loca_ :: (l)) (lit_ :: (Rational)) (litStr_ :: (String)) =
    (let
     in  ( ))
sem_Literal_PrimChar :: (l) ->
                        (Char) ->
                        (String) ->
                        (T_Literal) (l)
sem_Literal_PrimChar (loca_ :: (l)) (lit_ :: (Char)) (litStr_ :: (String)) =
    (let
     in  ( ))
sem_Literal_PrimString :: (l) ->
                          (String) ->
                          (String) ->
                          (T_Literal) (l)
sem_Literal_PrimString (loca_ :: (l)) (lit_ :: (String)) (litStr_ :: (String)) =
    (let
     in  ( ))
-- Match -------------------------------------------------------
-- cata
sem_Match :: (Match) (l) ->
             (T_Match) (l)
sem_Match (Match _loca _name _pats _rhs _mBinds) =
    (sem_Match_Match _loca _name _pats _rhs _mBinds)
sem_Match (InfixMatch _loca _pat _name _pats _rhs _mBinds) =
    (sem_Match_InfixMatch _loca _pat _name _pats _rhs _mBinds)
-- semantic domain
type T_Match l = ( )
data Inh_Match l = Inh_Match {}
data Syn_Match l = Syn_Match {}
wrap_Match :: (T_Match) (l) ->
              (Inh_Match) (l) ->
              (Syn_Match) (l)
wrap_Match sem (Inh_Match) =
    (let ( ) = sem
     in  (Syn_Match))
sem_Match_Match :: (l) ->
                   (Name l) ->
                   ([Pat l]) ->
                   (Rhs l) ->
                   (Maybe (Binds l)) ->
                   (T_Match) (l)
sem_Match_Match (loca_ :: (l)) (name_ :: (Name l)) (pats_ :: ([Pat l])) (rhs_ :: (Rhs l)) (mBinds_ :: (Maybe (Binds l))) =
    (let
     in  ( ))
sem_Match_InfixMatch :: (l) ->
                        (Pat l) ->
                        (Name l) ->
                        ([Pat l]) ->
                        (Rhs l) ->
                        (Maybe (Binds l)) ->
                        (T_Match) (l)
sem_Match_InfixMatch (loca_ :: (l)) (pat_ :: (Pat l)) (name_ :: (Name l)) (pats_ :: ([Pat l])) (rhs_ :: (Rhs l)) (mBinds_ :: (Maybe (Binds l))) =
    (let
     in  ( ))
-- MaybePromotedName -------------------------------------------
-- cata
sem_MaybePromotedName :: (MaybePromotedName) (l) ->
                         (T_MaybePromotedName) (l)
sem_MaybePromotedName (PromotedName _loca _qName) =
    (sem_MaybePromotedName_PromotedName _loca _qName)
sem_MaybePromotedName (UnpromotedName _loca _qName) =
    (sem_MaybePromotedName_UnpromotedName _loca _qName)
-- semantic domain
type T_MaybePromotedName l = ( )
data Inh_MaybePromotedName l = Inh_MaybePromotedName {}
data Syn_MaybePromotedName l = Syn_MaybePromotedName {}
wrap_MaybePromotedName :: (T_MaybePromotedName) (l) ->
                          (Inh_MaybePromotedName) (l) ->
                          (Syn_MaybePromotedName) (l)
wrap_MaybePromotedName sem (Inh_MaybePromotedName) =
    (let ( ) = sem
     in  (Syn_MaybePromotedName))
sem_MaybePromotedName_PromotedName :: (l) ->
                                      (QName l) ->
                                      (T_MaybePromotedName) (l)
sem_MaybePromotedName_PromotedName (loca_ :: (l)) (qName_ :: (QName l)) =
    (let
     in  ( ))
sem_MaybePromotedName_UnpromotedName :: (l) ->
                                        (QName l) ->
                                        (T_MaybePromotedName) (l)
sem_MaybePromotedName_UnpromotedName (loca_ :: (l)) (qName_ :: (QName l)) =
    (let
     in  ( ))
-- Module ------------------------------------------------------
-- cata
sem_Module :: (Module) (l) ->
              (T_Module) (l)
sem_Module (Module _loca _mModuleHead _modulePragmas _importDecls _decls) =
    (sem_Module_Module _loca _mModuleHead _modulePragmas _importDecls _decls)
sem_Module (XmlPage _loca _moduleName _modulePragmas _xName _xAttrs _mExp _exps) =
    (sem_Module_XmlPage _loca _moduleName _modulePragmas _xName _xAttrs _mExp _exps)
sem_Module (XmlHybrid _loca _mModuleHead _modulePragmas _importDecls _decls _xName _xAttrs _mExp _exps) =
    (sem_Module_XmlHybrid _loca _mModuleHead _modulePragmas _importDecls _decls _xName _xAttrs _mExp _exps)
-- semantic domain
type T_Module l = ( )
data Inh_Module l = Inh_Module {}
data Syn_Module l = Syn_Module {}
wrap_Module :: (T_Module) (l) ->
               (Inh_Module) (l) ->
               (Syn_Module) (l)
wrap_Module sem (Inh_Module) =
    (let ( ) = sem
     in  (Syn_Module))
sem_Module_Module :: (l) ->
                     (Maybe (ModuleHead l)) ->
                     ([ModulePragma l]) ->
                     ([ImportDecl l]) ->
                     ([Decl l]) ->
                     (T_Module) (l)
sem_Module_Module (loca_ :: (l)) (mModuleHead_ :: (Maybe (ModuleHead l))) (modulePragmas_ :: ([ModulePragma l])) (importDecls_ :: ([ImportDecl l])) (decls_ :: ([Decl l])) =
    (let
     in  ( ))
sem_Module_XmlPage :: (l) ->
                      (ModuleName l) ->
                      ([ModulePragma l]) ->
                      (XName l) ->
                      ([XAttr l]) ->
                      (Maybe (Exp l)) ->
                      ([Exp l]) ->
                      (T_Module) (l)
sem_Module_XmlPage (loca_ :: (l)) (moduleName_ :: (ModuleName l)) (modulePragmas_ :: ([ModulePragma l])) (xName_ :: (XName l)) (xAttrs_ :: ([XAttr l])) (mExp_ :: (Maybe (Exp l))) (exps_ :: ([Exp l])) =
    (let
     in  ( ))
sem_Module_XmlHybrid :: (l) ->
                        (Maybe (ModuleHead l)) ->
                        ([ModulePragma l]) ->
                        ([ImportDecl l]) ->
                        ([Decl l]) ->
                        (XName l) ->
                        ([XAttr l]) ->
                        (Maybe (Exp l)) ->
                        ([Exp l]) ->
                        (T_Module) (l)
sem_Module_XmlHybrid (loca_ :: (l)) (mModuleHead_ :: (Maybe (ModuleHead l))) (modulePragmas_ :: ([ModulePragma l])) (importDecls_ :: ([ImportDecl l])) (decls_ :: ([Decl l])) (xName_ :: (XName l)) (xAttrs_ :: ([XAttr l])) (mExp_ :: (Maybe (Exp l))) (exps_ :: ([Exp l])) =
    (let
     in  ( ))
-- ModuleHead --------------------------------------------------
-- cata
sem_ModuleHead :: (ModuleHead) (l) ->
                  (T_ModuleHead) (l)
sem_ModuleHead (ModuleHead _loca _moduleName _mWarningText _mExportSpecList) =
    (sem_ModuleHead_ModuleHead _loca _moduleName _mWarningText _mExportSpecList)
-- semantic domain
type T_ModuleHead l = ( )
data Inh_ModuleHead l = Inh_ModuleHead {}
data Syn_ModuleHead l = Syn_ModuleHead {}
wrap_ModuleHead :: (T_ModuleHead) (l) ->
                   (Inh_ModuleHead) (l) ->
                   (Syn_ModuleHead) (l)
wrap_ModuleHead sem (Inh_ModuleHead) =
    (let ( ) = sem
     in  (Syn_ModuleHead))
sem_ModuleHead_ModuleHead :: (l) ->
                             (ModuleName l) ->
                             (Maybe (WarningText l)) ->
                             (Maybe (ExportSpecList l)) ->
                             (T_ModuleHead) (l)
sem_ModuleHead_ModuleHead (loca_ :: (l)) (moduleName_ :: (ModuleName l)) (mWarningText_ :: (Maybe (WarningText l))) (mExportSpecList_ :: (Maybe (ExportSpecList l))) =
    (let
     in  ( ))
-- ModuleName --------------------------------------------------
-- cata
sem_ModuleName :: (ModuleName) (l) ->
                  (T_ModuleName) (l)
sem_ModuleName (ModuleName _loca _moduleName) =
    (sem_ModuleName_ModuleName _loca _moduleName)
-- semantic domain
type T_ModuleName l = ( )
data Inh_ModuleName l = Inh_ModuleName {}
data Syn_ModuleName l = Syn_ModuleName {}
wrap_ModuleName :: (T_ModuleName) (l) ->
                   (Inh_ModuleName) (l) ->
                   (Syn_ModuleName) (l)
wrap_ModuleName sem (Inh_ModuleName) =
    (let ( ) = sem
     in  (Syn_ModuleName))
sem_ModuleName_ModuleName :: (l) ->
                             (String) ->
                             (T_ModuleName) (l)
sem_ModuleName_ModuleName (loca_ :: (l)) (moduleName_ :: (String)) =
    (let
     in  ( ))
-- ModulePragma ------------------------------------------------
-- cata
sem_ModulePragma :: (ModulePragma) (l) ->
                    (T_ModulePragma) (l)
sem_ModulePragma (LanguagePragma _loca _names) =
    (sem_ModulePragma_LanguagePragma _loca _names)
sem_ModulePragma (OptionsPragma _loca _mTool _optionsPragmaStr) =
    (sem_ModulePragma_OptionsPragma _loca _mTool _optionsPragmaStr)
sem_ModulePragma (AnnModulePragma _loca _annPragma) =
    (sem_ModulePragma_AnnModulePragma _loca _annPragma)
-- semantic domain
type T_ModulePragma l = ( )
data Inh_ModulePragma l = Inh_ModulePragma {}
data Syn_ModulePragma l = Syn_ModulePragma {}
wrap_ModulePragma :: (T_ModulePragma) (l) ->
                     (Inh_ModulePragma) (l) ->
                     (Syn_ModulePragma) (l)
wrap_ModulePragma sem (Inh_ModulePragma) =
    (let ( ) = sem
     in  (Syn_ModulePragma))
sem_ModulePragma_LanguagePragma :: (l) ->
                                   ([Name l]) ->
                                   (T_ModulePragma) (l)
sem_ModulePragma_LanguagePragma (loca_ :: (l)) (names_ :: ([Name l])) =
    (let
     in  ( ))
sem_ModulePragma_OptionsPragma :: (l) ->
                                  (Maybe Tool) ->
                                  (String) ->
                                  (T_ModulePragma) (l)
sem_ModulePragma_OptionsPragma (loca_ :: (l)) (mTool_ :: (Maybe Tool)) (optionsPragmaStr_ :: (String)) =
    (let
     in  ( ))
sem_ModulePragma_AnnModulePragma :: (l) ->
                                    (Annotation l) ->
                                    (T_ModulePragma) (l)
sem_ModulePragma_AnnModulePragma (loca_ :: (l)) (annPragma_ :: (Annotation l)) =
    (let
     in  ( ))
-- Name --------------------------------------------------------
-- cata
sem_Name :: (Name) (l) ->
            (T_Name) (l)
sem_Name (Ident _loca _ident) =
    (sem_Name_Ident _loca _ident)
sem_Name (Symbol _loca _symbol) =
    (sem_Name_Symbol _loca _symbol)
-- semantic domain
type T_Name l = ( )
data Inh_Name l = Inh_Name {}
data Syn_Name l = Syn_Name {}
wrap_Name :: (T_Name) (l) ->
             (Inh_Name) (l) ->
             (Syn_Name) (l)
wrap_Name sem (Inh_Name) =
    (let ( ) = sem
     in  (Syn_Name))
sem_Name_Ident :: (l) ->
                  (String) ->
                  (T_Name) (l)
sem_Name_Ident (loca_ :: (l)) (ident_ :: (String)) =
    (let
     in  ( ))
sem_Name_Symbol :: (l) ->
                   (String) ->
                   (T_Name) (l)
sem_Name_Symbol (loca_ :: (l)) (symbol_ :: (String)) =
    (let
     in  ( ))
-- Namespace ---------------------------------------------------
-- cata
sem_Namespace :: (Namespace) (l) ->
                 (T_Namespace) (l)
sem_Namespace (NoNamespace _loca) =
    (sem_Namespace_NoNamespace _loca)
sem_Namespace (TypeNamespace _loca) =
    (sem_Namespace_TypeNamespace _loca)
sem_Namespace (PatternNamespace _loca) =
    (sem_Namespace_PatternNamespace _loca)
-- semantic domain
type T_Namespace l = ( )
data Inh_Namespace l = Inh_Namespace {}
data Syn_Namespace l = Syn_Namespace {}
wrap_Namespace :: (T_Namespace) (l) ->
                  (Inh_Namespace) (l) ->
                  (Syn_Namespace) (l)
wrap_Namespace sem (Inh_Namespace) =
    (let ( ) = sem
     in  (Syn_Namespace))
sem_Namespace_NoNamespace :: (l) ->
                             (T_Namespace) (l)
sem_Namespace_NoNamespace (loca_ :: (l)) =
    (let
     in  ( ))
sem_Namespace_TypeNamespace :: (l) ->
                               (T_Namespace) (l)
sem_Namespace_TypeNamespace (loca_ :: (l)) =
    (let
     in  ( ))
sem_Namespace_PatternNamespace :: (l) ->
                                  (T_Namespace) (l)
sem_Namespace_PatternNamespace (loca_ :: (l)) =
    (let
     in  ( ))
-- Op ----------------------------------------------------------
-- cata
sem_Op :: (Op) (l) ->
          (T_Op) (l)
sem_Op (VarOp _loca _varOp) =
    (sem_Op_VarOp _loca _varOp)
sem_Op (ConOp _loca _conOp) =
    (sem_Op_ConOp _loca _conOp)
-- semantic domain
type T_Op l = ( )
data Inh_Op l = Inh_Op {}
data Syn_Op l = Syn_Op {}
wrap_Op :: (T_Op) (l) ->
           (Inh_Op) (l) ->
           (Syn_Op) (l)
wrap_Op sem (Inh_Op) =
    (let ( ) = sem
     in  (Syn_Op))
sem_Op_VarOp :: (l) ->
                (Name l) ->
                (T_Op) (l)
sem_Op_VarOp (loca_ :: (l)) (varOp_ :: (Name l)) =
    (let
     in  ( ))
sem_Op_ConOp :: (l) ->
                (Name l) ->
                (T_Op) (l)
sem_Op_ConOp (loca_ :: (l)) (conOp_ :: (Name l)) =
    (let
     in  ( ))
-- Overlap -----------------------------------------------------
-- cata
sem_Overlap :: (Overlap) (l) ->
               (T_Overlap) (l)
sem_Overlap (NoOverlap _loca) =
    (sem_Overlap_NoOverlap _loca)
sem_Overlap (Overlap _loca) =
    (sem_Overlap_Overlap _loca)
sem_Overlap (Overlapping _loca) =
    (sem_Overlap_Overlapping _loca)
sem_Overlap (Overlaps _loca) =
    (sem_Overlap_Overlaps _loca)
sem_Overlap (Overlappable _loca) =
    (sem_Overlap_Overlappable _loca)
sem_Overlap (Incoherent _loca) =
    (sem_Overlap_Incoherent _loca)
-- semantic domain
type T_Overlap l = ( )
data Inh_Overlap l = Inh_Overlap {}
data Syn_Overlap l = Syn_Overlap {}
wrap_Overlap :: (T_Overlap) (l) ->
                (Inh_Overlap) (l) ->
                (Syn_Overlap) (l)
wrap_Overlap sem (Inh_Overlap) =
    (let ( ) = sem
     in  (Syn_Overlap))
sem_Overlap_NoOverlap :: (l) ->
                         (T_Overlap) (l)
sem_Overlap_NoOverlap (loca_ :: (l)) =
    (let
     in  ( ))
sem_Overlap_Overlap :: (l) ->
                       (T_Overlap) (l)
sem_Overlap_Overlap (loca_ :: (l)) =
    (let
     in  ( ))
sem_Overlap_Overlapping :: (l) ->
                           (T_Overlap) (l)
sem_Overlap_Overlapping (loca_ :: (l)) =
    (let
     in  ( ))
sem_Overlap_Overlaps :: (l) ->
                        (T_Overlap) (l)
sem_Overlap_Overlaps (loca_ :: (l)) =
    (let
     in  ( ))
sem_Overlap_Overlappable :: (l) ->
                            (T_Overlap) (l)
sem_Overlap_Overlappable (loca_ :: (l)) =
    (let
     in  ( ))
sem_Overlap_Incoherent :: (l) ->
                          (T_Overlap) (l)
sem_Overlap_Incoherent (loca_ :: (l)) =
    (let
     in  ( ))
-- PXAttr ------------------------------------------------------
-- cata
sem_PXAttr :: (PXAttr) (l) ->
              (T_PXAttr) (l)
sem_PXAttr (PXAttr _loca _xName _pat) =
    (sem_PXAttr_PXAttr _loca _xName _pat)
-- semantic domain
type T_PXAttr l = ( )
data Inh_PXAttr l = Inh_PXAttr {}
data Syn_PXAttr l = Syn_PXAttr {}
wrap_PXAttr :: (T_PXAttr) (l) ->
               (Inh_PXAttr) (l) ->
               (Syn_PXAttr) (l)
wrap_PXAttr sem (Inh_PXAttr) =
    (let ( ) = sem
     in  (Syn_PXAttr))
sem_PXAttr_PXAttr :: (l) ->
                     (XName l) ->
                     (Pat l) ->
                     (T_PXAttr) (l)
sem_PXAttr_PXAttr (loca_ :: (l)) (xName_ :: (XName l)) (pat_ :: (Pat l)) =
    (let
     in  ( ))
-- Pat ---------------------------------------------------------
-- cata
sem_Pat :: (Pat) (l) ->
           (T_Pat) (l)
sem_Pat (PVar _loca _name) =
    (sem_Pat_PVar _loca _name)
sem_Pat (PLit _loca _sign _literal) =
    (sem_Pat_PLit _loca _sign _literal)
sem_Pat (PNPlusK _loca _name _integer) =
    (sem_Pat_PNPlusK _loca _name _integer)
sem_Pat (PInfixApp _loca _pat1 _qName _pat2) =
    (sem_Pat_PInfixApp _loca _pat1 _qName _pat2)
sem_Pat (PApp _loca _qName _pats) =
    (sem_Pat_PApp _loca _qName _pats)
sem_Pat (PTuple _loca _boxed _pats) =
    (sem_Pat_PTuple _loca (sem_Boxed _boxed) _pats)
sem_Pat (PUnboxedSum _loca _int1 _int2 _pat) =
    (sem_Pat_PUnboxedSum _loca _int1 _int2 _pat)
sem_Pat (PList _loca _pats) =
    (sem_Pat_PList _loca _pats)
sem_Pat (PParen _loca _pat) =
    (sem_Pat_PParen _loca _pat)
sem_Pat (PRec _loca _qName _patFields) =
    (sem_Pat_PRec _loca _qName _patFields)
sem_Pat (PAsPat _loca _name _pat) =
    (sem_Pat_PAsPat _loca _name _pat)
sem_Pat (PWildCard _loca) =
    (sem_Pat_PWildCard _loca)
sem_Pat (PIrrPat _loca _pat) =
    (sem_Pat_PIrrPat _loca _pat)
sem_Pat (PatTypeSig _loca _pat _typ) =
    (sem_Pat_PatTypeSig _loca _pat _typ)
sem_Pat (PViewPat _loca _exp _pat) =
    (sem_Pat_PViewPat _loca _exp _pat)
sem_Pat (PRPat _loca _rPats) =
    (sem_Pat_PRPat _loca _rPats)
sem_Pat (PXTag _loca _xName _pXAttrs _mPat _pats) =
    (sem_Pat_PXTag _loca _xName _pXAttrs _mPat _pats)
sem_Pat (PXETag _loca _xName _pXAttrs _mPat) =
    (sem_Pat_PXETag _loca _xName _pXAttrs _mPat)
sem_Pat (PXPcdata _loca _string) =
    (sem_Pat_PXPcdata _loca _string)
sem_Pat (PXPatTag _loca _pat) =
    (sem_Pat_PXPatTag _loca _pat)
sem_Pat (PXRPats _loca _rPats) =
    (sem_Pat_PXRPats _loca _rPats)
sem_Pat (PSplice _loca _splice) =
    (sem_Pat_PSplice _loca _splice)
sem_Pat (PQuasiQuote _loca _string1 _string2) =
    (sem_Pat_PQuasiQuote _loca _string1 _string2)
sem_Pat (PBangPat _loca _pat) =
    (sem_Pat_PBangPat _loca _pat)
-- semantic domain
type T_Pat l = ( )
data Inh_Pat l = Inh_Pat {}
data Syn_Pat l = Syn_Pat {}
wrap_Pat :: (T_Pat) (l) ->
            (Inh_Pat) (l) ->
            (Syn_Pat) (l)
wrap_Pat sem (Inh_Pat) =
    (let ( ) = sem
     in  (Syn_Pat))
sem_Pat_PVar :: (l) ->
                (Name l) ->
                (T_Pat) (l)
sem_Pat_PVar (loca_ :: (l)) (name_ :: (Name l)) =
    (let
     in  ( ))
sem_Pat_PLit :: (l) ->
                (Sign l) ->
                (Literal l) ->
                (T_Pat) (l)
sem_Pat_PLit (loca_ :: (l)) (sign_ :: (Sign l)) (literal_ :: (Literal l)) =
    (let
     in  ( ))
sem_Pat_PNPlusK :: (l) ->
                   (Name l) ->
                   (Integer) ->
                   (T_Pat) (l)
sem_Pat_PNPlusK (loca_ :: (l)) (name_ :: (Name l)) (integer_ :: (Integer)) =
    (let
     in  ( ))
sem_Pat_PInfixApp :: (l) ->
                     (Pat l) ->
                     (QName l) ->
                     (Pat l) ->
                     (T_Pat) (l)
sem_Pat_PInfixApp (loca_ :: (l)) (pat1_ :: (Pat l)) (qName_ :: (QName l)) (pat2_ :: (Pat l)) =
    (let
     in  ( ))
sem_Pat_PApp :: (l) ->
                (QName l) ->
                ([Pat l]) ->
                (T_Pat) (l)
sem_Pat_PApp (loca_ :: (l)) (qName_ :: (QName l)) (pats_ :: ([Pat l])) =
    (let
     in  ( ))
sem_Pat_PTuple :: (l) ->
                  (T_Boxed) ->
                  ([Pat l]) ->
                  (T_Pat) (l)
sem_Pat_PTuple (loca_ :: (l)) boxed_ (pats_ :: ([Pat l])) =
    (let
     in  ( ))
sem_Pat_PUnboxedSum :: (l) ->
                       (Int) ->
                       (Int) ->
                       (Pat l) ->
                       (T_Pat) (l)
sem_Pat_PUnboxedSum (loca_ :: (l)) (int1_ :: (Int)) (int2_ :: (Int)) (pat_ :: (Pat l)) =
    (let
     in  ( ))
sem_Pat_PList :: (l) ->
                 ([Pat l]) ->
                 (T_Pat) (l)
sem_Pat_PList (loca_ :: (l)) (pats_ :: ([Pat l])) =
    (let
     in  ( ))
sem_Pat_PParen :: (l) ->
                  (Pat l) ->
                  (T_Pat) (l)
sem_Pat_PParen (loca_ :: (l)) (pat_ :: (Pat l)) =
    (let
     in  ( ))
sem_Pat_PRec :: (l) ->
                (QName l) ->
                ([PatField l]) ->
                (T_Pat) (l)
sem_Pat_PRec (loca_ :: (l)) (qName_ :: (QName l)) (patFields_ :: ([PatField l])) =
    (let
     in  ( ))
sem_Pat_PAsPat :: (l) ->
                  (Name l) ->
                  (Pat l) ->
                  (T_Pat) (l)
sem_Pat_PAsPat (loca_ :: (l)) (name_ :: (Name l)) (pat_ :: (Pat l)) =
    (let
     in  ( ))
sem_Pat_PWildCard :: (l) ->
                     (T_Pat) (l)
sem_Pat_PWildCard (loca_ :: (l)) =
    (let
     in  ( ))
sem_Pat_PIrrPat :: (l) ->
                   (Pat l) ->
                   (T_Pat) (l)
sem_Pat_PIrrPat (loca_ :: (l)) (pat_ :: (Pat l)) =
    (let
     in  ( ))
sem_Pat_PatTypeSig :: (l) ->
                      (Pat l) ->
                      (Type l) ->
                      (T_Pat) (l)
sem_Pat_PatTypeSig (loca_ :: (l)) (pat_ :: (Pat l)) (typ_ :: (Type l)) =
    (let
     in  ( ))
sem_Pat_PViewPat :: (l) ->
                    (Exp l) ->
                    (Pat l) ->
                    (T_Pat) (l)
sem_Pat_PViewPat (loca_ :: (l)) (exp_ :: (Exp l)) (pat_ :: (Pat l)) =
    (let
     in  ( ))
sem_Pat_PRPat :: (l) ->
                 ([RPat l]) ->
                 (T_Pat) (l)
sem_Pat_PRPat (loca_ :: (l)) (rPats_ :: ([RPat l])) =
    (let
     in  ( ))
sem_Pat_PXTag :: (l) ->
                 (XName l) ->
                 ([PXAttr l]) ->
                 (Maybe (Pat l)) ->
                 ([Pat l]) ->
                 (T_Pat) (l)
sem_Pat_PXTag (loca_ :: (l)) (xName_ :: (XName l)) (pXAttrs_ :: ([PXAttr l])) (mPat_ :: (Maybe (Pat l))) (pats_ :: ([Pat l])) =
    (let
     in  ( ))
sem_Pat_PXETag :: (l) ->
                  (XName l) ->
                  ([PXAttr l]) ->
                  (Maybe (Pat l)) ->
                  (T_Pat) (l)
sem_Pat_PXETag (loca_ :: (l)) (xName_ :: (XName l)) (pXAttrs_ :: ([PXAttr l])) (mPat_ :: (Maybe (Pat l))) =
    (let
     in  ( ))
sem_Pat_PXPcdata :: (l) ->
                    (String) ->
                    (T_Pat) (l)
sem_Pat_PXPcdata (loca_ :: (l)) (string_ :: (String)) =
    (let
     in  ( ))
sem_Pat_PXPatTag :: (l) ->
                    (Pat l) ->
                    (T_Pat) (l)
sem_Pat_PXPatTag (loca_ :: (l)) (pat_ :: (Pat l)) =
    (let
     in  ( ))
sem_Pat_PXRPats :: (l) ->
                   ([RPat l]) ->
                   (T_Pat) (l)
sem_Pat_PXRPats (loca_ :: (l)) (rPats_ :: ([RPat l])) =
    (let
     in  ( ))
sem_Pat_PSplice :: (l) ->
                   (Splice l) ->
                   (T_Pat) (l)
sem_Pat_PSplice (loca_ :: (l)) (splice_ :: (Splice l)) =
    (let
     in  ( ))
sem_Pat_PQuasiQuote :: (l) ->
                       (String) ->
                       (String) ->
                       (T_Pat) (l)
sem_Pat_PQuasiQuote (loca_ :: (l)) (string1_ :: (String)) (string2_ :: (String)) =
    (let
     in  ( ))
sem_Pat_PBangPat :: (l) ->
                    (Pat l) ->
                    (T_Pat) (l)
sem_Pat_PBangPat (loca_ :: (l)) (pat_ :: (Pat l)) =
    (let
     in  ( ))
-- PatField ----------------------------------------------------
-- cata
sem_PatField :: (PatField) (l) ->
                (T_PatField) (l)
sem_PatField (PFieldPat _loca _qName _pat) =
    (sem_PatField_PFieldPat _loca _qName _pat)
sem_PatField (PFieldPun _loca _qName) =
    (sem_PatField_PFieldPun _loca _qName)
sem_PatField (PFieldWildcard _loca) =
    (sem_PatField_PFieldWildcard _loca)
-- semantic domain
type T_PatField l = ( )
data Inh_PatField l = Inh_PatField {}
data Syn_PatField l = Syn_PatField {}
wrap_PatField :: (T_PatField) (l) ->
                 (Inh_PatField) (l) ->
                 (Syn_PatField) (l)
wrap_PatField sem (Inh_PatField) =
    (let ( ) = sem
     in  (Syn_PatField))
sem_PatField_PFieldPat :: (l) ->
                          (QName l) ->
                          (Pat l) ->
                          (T_PatField) (l)
sem_PatField_PFieldPat (loca_ :: (l)) (qName_ :: (QName l)) (pat_ :: (Pat l)) =
    (let
     in  ( ))
sem_PatField_PFieldPun :: (l) ->
                          (QName l) ->
                          (T_PatField) (l)
sem_PatField_PFieldPun (loca_ :: (l)) (qName_ :: (QName l)) =
    (let
     in  ( ))
sem_PatField_PFieldWildcard :: (l) ->
                               (T_PatField) (l)
sem_PatField_PFieldWildcard (loca_ :: (l)) =
    (let
     in  ( ))
-- PatternSynDirection -----------------------------------------
-- cata
sem_PatternSynDirection :: (PatternSynDirection) (l) ->
                           (T_PatternSynDirection) (l)
sem_PatternSynDirection (Unidirectional) =
    (sem_PatternSynDirection_Unidirectional)
sem_PatternSynDirection (ImplicitBidirectional) =
    (sem_PatternSynDirection_ImplicitBidirectional)
sem_PatternSynDirection (ExplicitBidirectional _loca _decls) =
    (sem_PatternSynDirection_ExplicitBidirectional _loca _decls)
-- semantic domain
type T_PatternSynDirection l = ( )
data Inh_PatternSynDirection l = Inh_PatternSynDirection {}
data Syn_PatternSynDirection l = Syn_PatternSynDirection {}
wrap_PatternSynDirection :: (T_PatternSynDirection) (l) ->
                            (Inh_PatternSynDirection) (l) ->
                            (Syn_PatternSynDirection) (l)
wrap_PatternSynDirection sem (Inh_PatternSynDirection) =
    (let ( ) = sem
     in  (Syn_PatternSynDirection))
sem_PatternSynDirection_Unidirectional :: (T_PatternSynDirection) (l)
sem_PatternSynDirection_Unidirectional =
    (let
     in  ( ))
sem_PatternSynDirection_ImplicitBidirectional :: (T_PatternSynDirection) (l)
sem_PatternSynDirection_ImplicitBidirectional =
    (let
     in  ( ))
sem_PatternSynDirection_ExplicitBidirectional :: (l) ->
                                                 ([Decl l]) ->
                                                 (T_PatternSynDirection) (l)
sem_PatternSynDirection_ExplicitBidirectional (loca_ :: (l)) (decls_ :: ([Decl l])) =
    (let
     in  ( ))
-- Promoted ----------------------------------------------------
-- cata
sem_Promoted :: (Promoted) (l) ->
                (T_Promoted) (l)
sem_Promoted (PromotedInteger _loca _integer _str) =
    (sem_Promoted_PromotedInteger _loca _integer _str)
sem_Promoted (PromotedString _loca _str1 _str2) =
    (sem_Promoted_PromotedString _loca _str1 _str2)
sem_Promoted (PromotedCon _loca _b _qName) =
    (sem_Promoted_PromotedCon _loca _b _qName)
sem_Promoted (PromotedList _loca _b _types) =
    (sem_Promoted_PromotedList _loca _b _types)
sem_Promoted (PromotedTuple _loca _types) =
    (sem_Promoted_PromotedTuple _loca _types)
sem_Promoted (PromotedUnit _loca) =
    (sem_Promoted_PromotedUnit _loca)
-- semantic domain
type T_Promoted l = ( )
data Inh_Promoted l = Inh_Promoted {}
data Syn_Promoted l = Syn_Promoted {}
wrap_Promoted :: (T_Promoted) (l) ->
                 (Inh_Promoted) (l) ->
                 (Syn_Promoted) (l)
wrap_Promoted sem (Inh_Promoted) =
    (let ( ) = sem
     in  (Syn_Promoted))
sem_Promoted_PromotedInteger :: (l) ->
                                (Integer) ->
                                (String) ->
                                (T_Promoted) (l)
sem_Promoted_PromotedInteger (loca_ :: (l)) (integer_ :: (Integer)) (str_ :: (String)) =
    (let
     in  ( ))
sem_Promoted_PromotedString :: (l) ->
                               (String) ->
                               (String) ->
                               (T_Promoted) (l)
sem_Promoted_PromotedString (loca_ :: (l)) (str1_ :: (String)) (str2_ :: (String)) =
    (let
     in  ( ))
sem_Promoted_PromotedCon :: (l) ->
                            (Bool) ->
                            (QName l) ->
                            (T_Promoted) (l)
sem_Promoted_PromotedCon (loca_ :: (l)) (b_ :: (Bool)) (qName_ :: (QName l)) =
    (let
     in  ( ))
sem_Promoted_PromotedList :: (l) ->
                             (Bool) ->
                             ([Type l]) ->
                             (T_Promoted) (l)
sem_Promoted_PromotedList (loca_ :: (l)) (b_ :: (Bool)) (types_ :: ([Type l])) =
    (let
     in  ( ))
sem_Promoted_PromotedTuple :: (l) ->
                              ([Type l]) ->
                              (T_Promoted) (l)
sem_Promoted_PromotedTuple (loca_ :: (l)) (types_ :: ([Type l])) =
    (let
     in  ( ))
sem_Promoted_PromotedUnit :: (l) ->
                             (T_Promoted) (l)
sem_Promoted_PromotedUnit (loca_ :: (l)) =
    (let
     in  ( ))
-- QName -------------------------------------------------------
-- cata
sem_QName :: (QName) (l) ->
             (T_QName) (l)
sem_QName (Qual _loca _moduleName _name) =
    (sem_QName_Qual _loca _moduleName _name)
sem_QName (UnQual _loca _name) =
    (sem_QName_UnQual _loca _name)
sem_QName (Special _loca _specialCon) =
    (sem_QName_Special _loca _specialCon)
-- semantic domain
type T_QName l = ( )
data Inh_QName l = Inh_QName {}
data Syn_QName l = Syn_QName {}
wrap_QName :: (T_QName) (l) ->
              (Inh_QName) (l) ->
              (Syn_QName) (l)
wrap_QName sem (Inh_QName) =
    (let ( ) = sem
     in  (Syn_QName))
sem_QName_Qual :: (l) ->
                  (ModuleName l) ->
                  (Name l) ->
                  (T_QName) (l)
sem_QName_Qual (loca_ :: (l)) (moduleName_ :: (ModuleName l)) (name_ :: (Name l)) =
    (let
     in  ( ))
sem_QName_UnQual :: (l) ->
                    (Name l) ->
                    (T_QName) (l)
sem_QName_UnQual (loca_ :: (l)) (name_ :: (Name l)) =
    (let
     in  ( ))
sem_QName_Special :: (l) ->
                     (SpecialCon l) ->
                     (T_QName) (l)
sem_QName_Special (loca_ :: (l)) (specialCon_ :: (SpecialCon l)) =
    (let
     in  ( ))
-- QOp ---------------------------------------------------------
-- cata
sem_QOp :: (QOp) (l) ->
           (T_QOp) (l)
sem_QOp (QVarOp _loca _qVarOp) =
    (sem_QOp_QVarOp _loca _qVarOp)
sem_QOp (QConOp _loca _qConOp) =
    (sem_QOp_QConOp _loca _qConOp)
-- semantic domain
type T_QOp l = ( )
data Inh_QOp l = Inh_QOp {}
data Syn_QOp l = Syn_QOp {}
wrap_QOp :: (T_QOp) (l) ->
            (Inh_QOp) (l) ->
            (Syn_QOp) (l)
wrap_QOp sem (Inh_QOp) =
    (let ( ) = sem
     in  (Syn_QOp))
sem_QOp_QVarOp :: (l) ->
                  (QName l) ->
                  (T_QOp) (l)
sem_QOp_QVarOp (loca_ :: (l)) (qVarOp_ :: (QName l)) =
    (let
     in  ( ))
sem_QOp_QConOp :: (l) ->
                  (QName l) ->
                  (T_QOp) (l)
sem_QOp_QConOp (loca_ :: (l)) (qConOp_ :: (QName l)) =
    (let
     in  ( ))
-- QualConDecl -------------------------------------------------
-- cata
sem_QualConDecl :: (QualConDecl) (l) ->
                   (T_QualConDecl) (l)
sem_QualConDecl (QualConDecl _loca _mTyVarBinds _mContext _conDecl) =
    (sem_QualConDecl_QualConDecl _loca _mTyVarBinds _mContext _conDecl)
-- semantic domain
type T_QualConDecl l = ( )
data Inh_QualConDecl l = Inh_QualConDecl {}
data Syn_QualConDecl l = Syn_QualConDecl {}
wrap_QualConDecl :: (T_QualConDecl) (l) ->
                    (Inh_QualConDecl) (l) ->
                    (Syn_QualConDecl) (l)
wrap_QualConDecl sem (Inh_QualConDecl) =
    (let ( ) = sem
     in  (Syn_QualConDecl))
sem_QualConDecl_QualConDecl :: (l) ->
                               (Maybe [TyVarBind l]) ->
                               (Maybe (Context l)) ->
                               (ConDecl l) ->
                               (T_QualConDecl) (l)
sem_QualConDecl_QualConDecl (loca_ :: (l)) (mTyVarBinds_ :: (Maybe [TyVarBind l])) (mContext_ :: (Maybe (Context l))) (conDecl_ :: (ConDecl l)) =
    (let
     in  ( ))
-- QualStmt ----------------------------------------------------
-- cata
sem_QualStmt :: (QualStmt) (l) ->
                (T_QualStmt) (l)
sem_QualStmt (QualStmt _loca _stmt) =
    (sem_QualStmt_QualStmt _loca _stmt)
sem_QualStmt (ThenTrans _loca _exp) =
    (sem_QualStmt_ThenTrans _loca _exp)
sem_QualStmt (ThenBy _loca _exp1 _exp2) =
    (sem_QualStmt_ThenBy _loca _exp1 _exp2)
sem_QualStmt (GroupBy _loca _exp) =
    (sem_QualStmt_GroupBy _loca _exp)
sem_QualStmt (GroupUsing _loca _exp) =
    (sem_QualStmt_GroupUsing _loca _exp)
sem_QualStmt (GroupByUsing _loca _exp1 _exp2) =
    (sem_QualStmt_GroupByUsing _loca _exp1 _exp2)
-- semantic domain
type T_QualStmt l = ( )
data Inh_QualStmt l = Inh_QualStmt {}
data Syn_QualStmt l = Syn_QualStmt {}
wrap_QualStmt :: (T_QualStmt) (l) ->
                 (Inh_QualStmt) (l) ->
                 (Syn_QualStmt) (l)
wrap_QualStmt sem (Inh_QualStmt) =
    (let ( ) = sem
     in  (Syn_QualStmt))
sem_QualStmt_QualStmt :: (l) ->
                         (Stmt l) ->
                         (T_QualStmt) (l)
sem_QualStmt_QualStmt (loca_ :: (l)) (stmt_ :: (Stmt l)) =
    (let
     in  ( ))
sem_QualStmt_ThenTrans :: (l) ->
                          (Exp l) ->
                          (T_QualStmt) (l)
sem_QualStmt_ThenTrans (loca_ :: (l)) (exp_ :: (Exp l)) =
    (let
     in  ( ))
sem_QualStmt_ThenBy :: (l) ->
                       (Exp l) ->
                       (Exp l) ->
                       (T_QualStmt) (l)
sem_QualStmt_ThenBy (loca_ :: (l)) (exp1_ :: (Exp l)) (exp2_ :: (Exp l)) =
    (let
     in  ( ))
sem_QualStmt_GroupBy :: (l) ->
                        (Exp l) ->
                        (T_QualStmt) (l)
sem_QualStmt_GroupBy (loca_ :: (l)) (exp_ :: (Exp l)) =
    (let
     in  ( ))
sem_QualStmt_GroupUsing :: (l) ->
                           (Exp l) ->
                           (T_QualStmt) (l)
sem_QualStmt_GroupUsing (loca_ :: (l)) (exp_ :: (Exp l)) =
    (let
     in  ( ))
sem_QualStmt_GroupByUsing :: (l) ->
                             (Exp l) ->
                             (Exp l) ->
                             (T_QualStmt) (l)
sem_QualStmt_GroupByUsing (loca_ :: (l)) (exp1_ :: (Exp l)) (exp2_ :: (Exp l)) =
    (let
     in  ( ))
-- RPat --------------------------------------------------------
-- cata
sem_RPat :: (RPat) (l) ->
            (T_RPat) (l)
sem_RPat (RPOp _loca _rPat _rPatOp) =
    (sem_RPat_RPOp _loca _rPat _rPatOp)
sem_RPat (RPEither _loca _rPat1 _rPat2) =
    (sem_RPat_RPEither _loca _rPat1 _rPat2)
sem_RPat (RPSeq _loca _rPats) =
    (sem_RPat_RPSeq _loca _rPats)
sem_RPat (RPGuard _loca _pat _stmts) =
    (sem_RPat_RPGuard _loca _pat _stmts)
sem_RPat (RPCAs _loca _name _rPat) =
    (sem_RPat_RPCAs _loca _name _rPat)
sem_RPat (RPAs _loca _name _rPat) =
    (sem_RPat_RPAs _loca _name _rPat)
sem_RPat (RPParen _loca _rPat) =
    (sem_RPat_RPParen _loca _rPat)
sem_RPat (RPPat _loca _pat) =
    (sem_RPat_RPPat _loca _pat)
-- semantic domain
type T_RPat l = ( )
data Inh_RPat l = Inh_RPat {}
data Syn_RPat l = Syn_RPat {}
wrap_RPat :: (T_RPat) (l) ->
             (Inh_RPat) (l) ->
             (Syn_RPat) (l)
wrap_RPat sem (Inh_RPat) =
    (let ( ) = sem
     in  (Syn_RPat))
sem_RPat_RPOp :: (l) ->
                 (RPat l) ->
                 (RPatOp l) ->
                 (T_RPat) (l)
sem_RPat_RPOp (loca_ :: (l)) (rPat_ :: (RPat l)) (rPatOp_ :: (RPatOp l)) =
    (let
     in  ( ))
sem_RPat_RPEither :: (l) ->
                     (RPat l) ->
                     (RPat l) ->
                     (T_RPat) (l)
sem_RPat_RPEither (loca_ :: (l)) (rPat1_ :: (RPat l)) (rPat2_ :: (RPat l)) =
    (let
     in  ( ))
sem_RPat_RPSeq :: (l) ->
                  ([RPat l]) ->
                  (T_RPat) (l)
sem_RPat_RPSeq (loca_ :: (l)) (rPats_ :: ([RPat l])) =
    (let
     in  ( ))
sem_RPat_RPGuard :: (l) ->
                    (Pat l) ->
                    ([Stmt l]) ->
                    (T_RPat) (l)
sem_RPat_RPGuard (loca_ :: (l)) (pat_ :: (Pat l)) (stmts_ :: ([Stmt l])) =
    (let
     in  ( ))
sem_RPat_RPCAs :: (l) ->
                  (Name l) ->
                  (RPat l) ->
                  (T_RPat) (l)
sem_RPat_RPCAs (loca_ :: (l)) (name_ :: (Name l)) (rPat_ :: (RPat l)) =
    (let
     in  ( ))
sem_RPat_RPAs :: (l) ->
                 (Name l) ->
                 (RPat l) ->
                 (T_RPat) (l)
sem_RPat_RPAs (loca_ :: (l)) (name_ :: (Name l)) (rPat_ :: (RPat l)) =
    (let
     in  ( ))
sem_RPat_RPParen :: (l) ->
                    (RPat l) ->
                    (T_RPat) (l)
sem_RPat_RPParen (loca_ :: (l)) (rPat_ :: (RPat l)) =
    (let
     in  ( ))
sem_RPat_RPPat :: (l) ->
                  (Pat l) ->
                  (T_RPat) (l)
sem_RPat_RPPat (loca_ :: (l)) (pat_ :: (Pat l)) =
    (let
     in  ( ))
-- RPatOp ------------------------------------------------------
-- cata
sem_RPatOp :: (RPatOp) (l) ->
              (T_RPatOp) (l)
sem_RPatOp (RPStar _loca) =
    (sem_RPatOp_RPStar _loca)
sem_RPatOp (RPStarG _loca) =
    (sem_RPatOp_RPStarG _loca)
sem_RPatOp (RPPlus _loca) =
    (sem_RPatOp_RPPlus _loca)
sem_RPatOp (RPPlusG _loca) =
    (sem_RPatOp_RPPlusG _loca)
sem_RPatOp (RPOpt _loca) =
    (sem_RPatOp_RPOpt _loca)
sem_RPatOp (RPOptG _loca) =
    (sem_RPatOp_RPOptG _loca)
-- semantic domain
type T_RPatOp l = ( )
data Inh_RPatOp l = Inh_RPatOp {}
data Syn_RPatOp l = Syn_RPatOp {}
wrap_RPatOp :: (T_RPatOp) (l) ->
               (Inh_RPatOp) (l) ->
               (Syn_RPatOp) (l)
wrap_RPatOp sem (Inh_RPatOp) =
    (let ( ) = sem
     in  (Syn_RPatOp))
sem_RPatOp_RPStar :: (l) ->
                     (T_RPatOp) (l)
sem_RPatOp_RPStar (loca_ :: (l)) =
    (let
     in  ( ))
sem_RPatOp_RPStarG :: (l) ->
                      (T_RPatOp) (l)
sem_RPatOp_RPStarG (loca_ :: (l)) =
    (let
     in  ( ))
sem_RPatOp_RPPlus :: (l) ->
                     (T_RPatOp) (l)
sem_RPatOp_RPPlus (loca_ :: (l)) =
    (let
     in  ( ))
sem_RPatOp_RPPlusG :: (l) ->
                      (T_RPatOp) (l)
sem_RPatOp_RPPlusG (loca_ :: (l)) =
    (let
     in  ( ))
sem_RPatOp_RPOpt :: (l) ->
                    (T_RPatOp) (l)
sem_RPatOp_RPOpt (loca_ :: (l)) =
    (let
     in  ( ))
sem_RPatOp_RPOptG :: (l) ->
                     (T_RPatOp) (l)
sem_RPatOp_RPOptG (loca_ :: (l)) =
    (let
     in  ( ))
-- ResultSig ---------------------------------------------------
-- cata
sem_ResultSig :: (ResultSig) (l) ->
                 (T_ResultSig) (l)
sem_ResultSig (KindSig _loca _kind) =
    (sem_ResultSig_KindSig _loca _kind)
sem_ResultSig (TyVarSig _loca _tyVarBind) =
    (sem_ResultSig_TyVarSig _loca _tyVarBind)
-- semantic domain
type T_ResultSig l = ( )
data Inh_ResultSig l = Inh_ResultSig {}
data Syn_ResultSig l = Syn_ResultSig {}
wrap_ResultSig :: (T_ResultSig) (l) ->
                  (Inh_ResultSig) (l) ->
                  (Syn_ResultSig) (l)
wrap_ResultSig sem (Inh_ResultSig) =
    (let ( ) = sem
     in  (Syn_ResultSig))
sem_ResultSig_KindSig :: (l) ->
                         (Kind l) ->
                         (T_ResultSig) (l)
sem_ResultSig_KindSig (loca_ :: (l)) (kind_ :: (Kind l)) =
    (let
     in  ( ))
sem_ResultSig_TyVarSig :: (l) ->
                          (TyVarBind l) ->
                          (T_ResultSig) (l)
sem_ResultSig_TyVarSig (loca_ :: (l)) (tyVarBind_ :: (TyVarBind l)) =
    (let
     in  ( ))
-- Rhs ---------------------------------------------------------
-- cata
sem_Rhs :: (Rhs) (l) ->
           (T_Rhs) (l)
sem_Rhs (UnGuardedRhs _loca _exp) =
    (sem_Rhs_UnGuardedRhs _loca _exp)
sem_Rhs (GuardedRhss _loca _guardedRhss) =
    (sem_Rhs_GuardedRhss _loca _guardedRhss)
-- semantic domain
type T_Rhs l = ( )
data Inh_Rhs l = Inh_Rhs {}
data Syn_Rhs l = Syn_Rhs {}
wrap_Rhs :: (T_Rhs) (l) ->
            (Inh_Rhs) (l) ->
            (Syn_Rhs) (l)
wrap_Rhs sem (Inh_Rhs) =
    (let ( ) = sem
     in  (Syn_Rhs))
sem_Rhs_UnGuardedRhs :: (l) ->
                        (Exp l) ->
                        (T_Rhs) (l)
sem_Rhs_UnGuardedRhs (loca_ :: (l)) (exp_ :: (Exp l)) =
    (let
     in  ( ))
sem_Rhs_GuardedRhss :: (l) ->
                       ([GuardedRhs l]) ->
                       (T_Rhs) (l)
sem_Rhs_GuardedRhss (loca_ :: (l)) (guardedRhss_ :: ([GuardedRhs l])) =
    (let
     in  ( ))
-- Role --------------------------------------------------------
-- cata
sem_Role :: (Role) (l) ->
            (T_Role) (l)
sem_Role (Nominal _loca) =
    (sem_Role_Nominal _loca)
sem_Role (Representational _loca) =
    (sem_Role_Representational _loca)
sem_Role (Phantom _loca) =
    (sem_Role_Phantom _loca)
sem_Role (RoleWildcard _loca) =
    (sem_Role_RoleWildcard _loca)
-- semantic domain
type T_Role l = ( )
data Inh_Role l = Inh_Role {}
data Syn_Role l = Syn_Role {}
wrap_Role :: (T_Role) (l) ->
             (Inh_Role) (l) ->
             (Syn_Role) (l)
wrap_Role sem (Inh_Role) =
    (let ( ) = sem
     in  (Syn_Role))
sem_Role_Nominal :: (l) ->
                    (T_Role) (l)
sem_Role_Nominal (loca_ :: (l)) =
    (let
     in  ( ))
sem_Role_Representational :: (l) ->
                             (T_Role) (l)
sem_Role_Representational (loca_ :: (l)) =
    (let
     in  ( ))
sem_Role_Phantom :: (l) ->
                    (T_Role) (l)
sem_Role_Phantom (loca_ :: (l)) =
    (let
     in  ( ))
sem_Role_RoleWildcard :: (l) ->
                         (T_Role) (l)
sem_Role_RoleWildcard (loca_ :: (l)) =
    (let
     in  ( ))
-- Rule --------------------------------------------------------
-- cata
sem_Rule :: (Rule) (l) ->
            (T_Rule) (l)
sem_Rule (Rule _loca _string _mActivation _mRuleVars _exp1 _exp2) =
    (sem_Rule_Rule _loca _string _mActivation _mRuleVars _exp1 _exp2)
-- semantic domain
type T_Rule l = ( )
data Inh_Rule l = Inh_Rule {}
data Syn_Rule l = Syn_Rule {}
wrap_Rule :: (T_Rule) (l) ->
             (Inh_Rule) (l) ->
             (Syn_Rule) (l)
wrap_Rule sem (Inh_Rule) =
    (let ( ) = sem
     in  (Syn_Rule))
sem_Rule_Rule :: (l) ->
                 (String) ->
                 (Maybe (Activation l)) ->
                 (Maybe [RuleVar l]) ->
                 (Exp l) ->
                 (Exp l) ->
                 (T_Rule) (l)
sem_Rule_Rule (loca_ :: (l)) (string_ :: (String)) (mActivation_ :: (Maybe (Activation l))) (mRuleVars_ :: (Maybe [RuleVar l])) (exp1_ :: (Exp l)) (exp2_ :: (Exp l)) =
    (let
     in  ( ))
-- RuleVar -----------------------------------------------------
-- cata
sem_RuleVar :: (RuleVar) (l) ->
               (T_RuleVar) (l)
sem_RuleVar (RuleVar _loca _name) =
    (sem_RuleVar_RuleVar _loca _name)
sem_RuleVar (TypedRuleVar _loca _name _typ) =
    (sem_RuleVar_TypedRuleVar _loca _name _typ)
-- semantic domain
type T_RuleVar l = ( )
data Inh_RuleVar l = Inh_RuleVar {}
data Syn_RuleVar l = Syn_RuleVar {}
wrap_RuleVar :: (T_RuleVar) (l) ->
                (Inh_RuleVar) (l) ->
                (Syn_RuleVar) (l)
wrap_RuleVar sem (Inh_RuleVar) =
    (let ( ) = sem
     in  (Syn_RuleVar))
sem_RuleVar_RuleVar :: (l) ->
                       (Name l) ->
                       (T_RuleVar) (l)
sem_RuleVar_RuleVar (loca_ :: (l)) (name_ :: (Name l)) =
    (let
     in  ( ))
sem_RuleVar_TypedRuleVar :: (l) ->
                            (Name l) ->
                            (Type l) ->
                            (T_RuleVar) (l)
sem_RuleVar_TypedRuleVar (loca_ :: (l)) (name_ :: (Name l)) (typ_ :: (Type l)) =
    (let
     in  ( ))
-- Safety ------------------------------------------------------
-- cata
sem_Safety :: (Safety) (l) ->
              (T_Safety) (l)
sem_Safety (PlayRisky _loca) =
    (sem_Safety_PlayRisky _loca)
sem_Safety (PlaySafe _loca _bool) =
    (sem_Safety_PlaySafe _loca _bool)
sem_Safety (PlayInterruptible _loca) =
    (sem_Safety_PlayInterruptible _loca)
-- semantic domain
type T_Safety l = ( )
data Inh_Safety l = Inh_Safety {}
data Syn_Safety l = Syn_Safety {}
wrap_Safety :: (T_Safety) (l) ->
               (Inh_Safety) (l) ->
               (Syn_Safety) (l)
wrap_Safety sem (Inh_Safety) =
    (let ( ) = sem
     in  (Syn_Safety))
sem_Safety_PlayRisky :: (l) ->
                        (T_Safety) (l)
sem_Safety_PlayRisky (loca_ :: (l)) =
    (let
     in  ( ))
sem_Safety_PlaySafe :: (l) ->
                       (Bool) ->
                       (T_Safety) (l)
sem_Safety_PlaySafe (loca_ :: (l)) (bool_ :: (Bool)) =
    (let
     in  ( ))
sem_Safety_PlayInterruptible :: (l) ->
                                (T_Safety) (l)
sem_Safety_PlayInterruptible (loca_ :: (l)) =
    (let
     in  ( ))
-- Sign --------------------------------------------------------
-- cata
sem_Sign :: (Sign) (l) ->
            (T_Sign) (l)
sem_Sign (Signless _loca) =
    (sem_Sign_Signless _loca)
sem_Sign (Negative _loca) =
    (sem_Sign_Negative _loca)
-- semantic domain
type T_Sign l = ( )
data Inh_Sign l = Inh_Sign {}
data Syn_Sign l = Syn_Sign {}
wrap_Sign :: (T_Sign) (l) ->
             (Inh_Sign) (l) ->
             (Syn_Sign) (l)
wrap_Sign sem (Inh_Sign) =
    (let ( ) = sem
     in  (Syn_Sign))
sem_Sign_Signless :: (l) ->
                     (T_Sign) (l)
sem_Sign_Signless (loca_ :: (l)) =
    (let
     in  ( ))
sem_Sign_Negative :: (l) ->
                     (T_Sign) (l)
sem_Sign_Negative (loca_ :: (l)) =
    (let
     in  ( ))
-- SpecialCon --------------------------------------------------
-- cata
sem_SpecialCon :: (SpecialCon) (l) ->
                  (T_SpecialCon) (l)
sem_SpecialCon (UnitCon _loca) =
    (sem_SpecialCon_UnitCon _loca)
sem_SpecialCon (ListCon _loca) =
    (sem_SpecialCon_ListCon _loca)
sem_SpecialCon (FunCon _loca) =
    (sem_SpecialCon_FunCon _loca)
sem_SpecialCon (TupleCon _loca _b _ary) =
    (sem_SpecialCon_TupleCon _loca (sem_Boxed _b) _ary)
sem_SpecialCon (Cons _loca) =
    (sem_SpecialCon_Cons _loca)
sem_SpecialCon (UnboxedSingleCon _loca) =
    (sem_SpecialCon_UnboxedSingleCon _loca)
sem_SpecialCon (ExprHole _loca) =
    (sem_SpecialCon_ExprHole _loca)
-- semantic domain
type T_SpecialCon l = ( )
data Inh_SpecialCon l = Inh_SpecialCon {}
data Syn_SpecialCon l = Syn_SpecialCon {}
wrap_SpecialCon :: (T_SpecialCon) (l) ->
                   (Inh_SpecialCon) (l) ->
                   (Syn_SpecialCon) (l)
wrap_SpecialCon sem (Inh_SpecialCon) =
    (let ( ) = sem
     in  (Syn_SpecialCon))
sem_SpecialCon_UnitCon :: (l) ->
                          (T_SpecialCon) (l)
sem_SpecialCon_UnitCon (loca_ :: (l)) =
    (let
     in  ( ))
sem_SpecialCon_ListCon :: (l) ->
                          (T_SpecialCon) (l)
sem_SpecialCon_ListCon (loca_ :: (l)) =
    (let
     in  ( ))
sem_SpecialCon_FunCon :: (l) ->
                         (T_SpecialCon) (l)
sem_SpecialCon_FunCon (loca_ :: (l)) =
    (let
     in  ( ))
sem_SpecialCon_TupleCon :: (l) ->
                           (T_Boxed) ->
                           (Int) ->
                           (T_SpecialCon) (l)
sem_SpecialCon_TupleCon (loca_ :: (l)) b_ (ary_ :: (Int)) =
    (let
     in  ( ))
sem_SpecialCon_Cons :: (l) ->
                       (T_SpecialCon) (l)
sem_SpecialCon_Cons (loca_ :: (l)) =
    (let
     in  ( ))
sem_SpecialCon_UnboxedSingleCon :: (l) ->
                                   (T_SpecialCon) (l)
sem_SpecialCon_UnboxedSingleCon (loca_ :: (l)) =
    (let
     in  ( ))
sem_SpecialCon_ExprHole :: (l) ->
                           (T_SpecialCon) (l)
sem_SpecialCon_ExprHole (loca_ :: (l)) =
    (let
     in  ( ))
-- Splice ------------------------------------------------------
-- cata
sem_Splice :: (Splice) (l) ->
              (T_Splice) (l)
sem_Splice (IdSplice _loca _string) =
    (sem_Splice_IdSplice _loca _string)
sem_Splice (ParenSplice _loca _exp) =
    (sem_Splice_ParenSplice _loca _exp)
-- semantic domain
type T_Splice l = ( )
data Inh_Splice l = Inh_Splice {}
data Syn_Splice l = Syn_Splice {}
wrap_Splice :: (T_Splice) (l) ->
               (Inh_Splice) (l) ->
               (Syn_Splice) (l)
wrap_Splice sem (Inh_Splice) =
    (let ( ) = sem
     in  (Syn_Splice))
sem_Splice_IdSplice :: (l) ->
                       (String) ->
                       (T_Splice) (l)
sem_Splice_IdSplice (loca_ :: (l)) (string_ :: (String)) =
    (let
     in  ( ))
sem_Splice_ParenSplice :: (l) ->
                          (Exp l) ->
                          (T_Splice) (l)
sem_Splice_ParenSplice (loca_ :: (l)) (exp_ :: (Exp l)) =
    (let
     in  ( ))
-- Stmt --------------------------------------------------------
-- cata
sem_Stmt :: (Stmt) (l) ->
            (T_Stmt) (l)
sem_Stmt (Generator _loca _pat _exp) =
    (sem_Stmt_Generator _loca _pat _exp)
sem_Stmt (Qualifier _loca _exp) =
    (sem_Stmt_Qualifier _loca _exp)
sem_Stmt (LetStmt _loca _binds) =
    (sem_Stmt_LetStmt _loca _binds)
sem_Stmt (RecStmt _loca _stmts) =
    (sem_Stmt_RecStmt _loca _stmts)
-- semantic domain
type T_Stmt l = ( )
data Inh_Stmt l = Inh_Stmt {}
data Syn_Stmt l = Syn_Stmt {}
wrap_Stmt :: (T_Stmt) (l) ->
             (Inh_Stmt) (l) ->
             (Syn_Stmt) (l)
wrap_Stmt sem (Inh_Stmt) =
    (let ( ) = sem
     in  (Syn_Stmt))
sem_Stmt_Generator :: (l) ->
                      (Pat l) ->
                      (Exp l) ->
                      (T_Stmt) (l)
sem_Stmt_Generator (loca_ :: (l)) (pat_ :: (Pat l)) (exp_ :: (Exp l)) =
    (let
     in  ( ))
sem_Stmt_Qualifier :: (l) ->
                      (Exp l) ->
                      (T_Stmt) (l)
sem_Stmt_Qualifier (loca_ :: (l)) (exp_ :: (Exp l)) =
    (let
     in  ( ))
sem_Stmt_LetStmt :: (l) ->
                    (Binds l) ->
                    (T_Stmt) (l)
sem_Stmt_LetStmt (loca_ :: (l)) (binds_ :: (Binds l)) =
    (let
     in  ( ))
sem_Stmt_RecStmt :: (l) ->
                    ([Stmt l]) ->
                    (T_Stmt) (l)
sem_Stmt_RecStmt (loca_ :: (l)) (stmts_ :: ([Stmt l])) =
    (let
     in  ( ))
-- Tool --------------------------------------------------------
-- cata
sem_Tool :: Tool ->
            T_Tool
sem_Tool (GHC) =
    (sem_Tool_GHC)
sem_Tool (HUGS) =
    (sem_Tool_HUGS)
sem_Tool (NHC98) =
    (sem_Tool_NHC98)
sem_Tool (YHC) =
    (sem_Tool_YHC)
sem_Tool (HADDOCK) =
    (sem_Tool_HADDOCK)
sem_Tool (UnknownTool _toolName) =
    (sem_Tool_UnknownTool _toolName)
-- semantic domain
type T_Tool = ( )
data Inh_Tool = Inh_Tool {}
data Syn_Tool = Syn_Tool {}
wrap_Tool :: T_Tool ->
             Inh_Tool ->
             Syn_Tool
wrap_Tool sem (Inh_Tool) =
    (let ( ) = sem
     in  (Syn_Tool))
sem_Tool_GHC :: T_Tool
sem_Tool_GHC =
    (let
     in  ( ))
sem_Tool_HUGS :: T_Tool
sem_Tool_HUGS =
    (let
     in  ( ))
sem_Tool_NHC98 :: T_Tool
sem_Tool_NHC98 =
    (let
     in  ( ))
sem_Tool_YHC :: T_Tool
sem_Tool_YHC =
    (let
     in  ( ))
sem_Tool_HADDOCK :: T_Tool
sem_Tool_HADDOCK =
    (let
     in  ( ))
sem_Tool_UnknownTool :: String ->
                        T_Tool
sem_Tool_UnknownTool toolName_ =
    (let
     in  ( ))
-- TyVarBind ---------------------------------------------------
-- cata
sem_TyVarBind :: (TyVarBind) (l) ->
                 (T_TyVarBind) (l)
sem_TyVarBind (KindedVar _loca _name _kind) =
    (sem_TyVarBind_KindedVar _loca _name _kind)
sem_TyVarBind (UnkindedVar _loca _name) =
    (sem_TyVarBind_UnkindedVar _loca _name)
-- semantic domain
type T_TyVarBind l = ( )
data Inh_TyVarBind l = Inh_TyVarBind {}
data Syn_TyVarBind l = Syn_TyVarBind {}
wrap_TyVarBind :: (T_TyVarBind) (l) ->
                  (Inh_TyVarBind) (l) ->
                  (Syn_TyVarBind) (l)
wrap_TyVarBind sem (Inh_TyVarBind) =
    (let ( ) = sem
     in  (Syn_TyVarBind))
sem_TyVarBind_KindedVar :: (l) ->
                           (Name l) ->
                           (Kind l) ->
                           (T_TyVarBind) (l)
sem_TyVarBind_KindedVar (loca_ :: (l)) (name_ :: (Name l)) (kind_ :: (Kind l)) =
    (let
     in  ( ))
sem_TyVarBind_UnkindedVar :: (l) ->
                             (Name l) ->
                             (T_TyVarBind) (l)
sem_TyVarBind_UnkindedVar (loca_ :: (l)) (name_ :: (Name l)) =
    (let
     in  ( ))
-- Type --------------------------------------------------------
-- cata
sem_Type :: (Type) (l) ->
            (T_Type) (l)
sem_Type (TyForall _loca _mTyVarBinds _mContext _typ) =
    (sem_Type_TyForall _loca _mTyVarBinds _mContext _typ)
sem_Type (TyFun _loca _typ1 _typ2) =
    (sem_Type_TyFun _loca _typ1 _typ2)
sem_Type (TyTuple _loca _boxed _types) =
    (sem_Type_TyTuple _loca (sem_Boxed _boxed) _types)
sem_Type (TyUnboxedSum _loca _types) =
    (sem_Type_TyUnboxedSum _loca _types)
sem_Type (TyList _loca _typ) =
    (sem_Type_TyList _loca _typ)
sem_Type (TyParArray _loca _typ) =
    (sem_Type_TyParArray _loca _typ)
sem_Type (TyApp _loca _typ1 _typ2) =
    (sem_Type_TyApp _loca _typ1 _typ2)
sem_Type (TyVar _loca _name) =
    (sem_Type_TyVar _loca _name)
sem_Type (TyCon _loca _qName) =
    (sem_Type_TyCon _loca _qName)
sem_Type (TyParen _loca _typ) =
    (sem_Type_TyParen _loca _typ)
sem_Type (TyInfix _loca _typ1 _maybePromotedName _typ2) =
    (sem_Type_TyInfix _loca _typ1 _maybePromotedName _typ2)
sem_Type (TyKind _loca _typ _kind) =
    (sem_Type_TyKind _loca _typ _kind)
sem_Type (TyPromoted _loca _promoted) =
    (sem_Type_TyPromoted _loca _promoted)
sem_Type (TyEquals _loca _typ1 _typ2) =
    (sem_Type_TyEquals _loca _typ1 _typ2)
sem_Type (TySplice _loca _splice) =
    (sem_Type_TySplice _loca _splice)
sem_Type (TyBang _loca _bangType _unpackedness _typ) =
    (sem_Type_TyBang _loca _bangType _unpackedness _typ)
sem_Type (TyWildCard _loca _mName) =
    (sem_Type_TyWildCard _loca _mName)
sem_Type (TyQuasiQuote _loca _str1 _str2) =
    (sem_Type_TyQuasiQuote _loca _str1 _str2)
-- semantic domain
type T_Type l = ( )
data Inh_Type l = Inh_Type {}
data Syn_Type l = Syn_Type {}
wrap_Type :: (T_Type) (l) ->
             (Inh_Type) (l) ->
             (Syn_Type) (l)
wrap_Type sem (Inh_Type) =
    (let ( ) = sem
     in  (Syn_Type))
sem_Type_TyForall :: (l) ->
                     (Maybe [TyVarBind l]) ->
                     (Maybe (Context l)) ->
                     (Type l) ->
                     (T_Type) (l)
sem_Type_TyForall (loca_ :: (l)) (mTyVarBinds_ :: (Maybe [TyVarBind l])) (mContext_ :: (Maybe (Context l))) (typ_ :: (Type l)) =
    (let
     in  ( ))
sem_Type_TyFun :: (l) ->
                  (Type l) ->
                  (Type l) ->
                  (T_Type) (l)
sem_Type_TyFun (loca_ :: (l)) (typ1_ :: (Type l)) (typ2_ :: (Type l)) =
    (let
     in  ( ))
sem_Type_TyTuple :: (l) ->
                    (T_Boxed) ->
                    ([Type l]) ->
                    (T_Type) (l)
sem_Type_TyTuple (loca_ :: (l)) boxed_ (types_ :: ([Type l])) =
    (let
     in  ( ))
sem_Type_TyUnboxedSum :: (l) ->
                         ([Type l]) ->
                         (T_Type) (l)
sem_Type_TyUnboxedSum (loca_ :: (l)) (types_ :: ([Type l])) =
    (let
     in  ( ))
sem_Type_TyList :: (l) ->
                   (Type l) ->
                   (T_Type) (l)
sem_Type_TyList (loca_ :: (l)) (typ_ :: (Type l)) =
    (let
     in  ( ))
sem_Type_TyParArray :: (l) ->
                       (Type l) ->
                       (T_Type) (l)
sem_Type_TyParArray (loca_ :: (l)) (typ_ :: (Type l)) =
    (let
     in  ( ))
sem_Type_TyApp :: (l) ->
                  (Type l) ->
                  (Type l) ->
                  (T_Type) (l)
sem_Type_TyApp (loca_ :: (l)) (typ1_ :: (Type l)) (typ2_ :: (Type l)) =
    (let
     in  ( ))
sem_Type_TyVar :: (l) ->
                  (Name l) ->
                  (T_Type) (l)
sem_Type_TyVar (loca_ :: (l)) (name_ :: (Name l)) =
    (let
     in  ( ))
sem_Type_TyCon :: (l) ->
                  (QName l) ->
                  (T_Type) (l)
sem_Type_TyCon (loca_ :: (l)) (qName_ :: (QName l)) =
    (let
     in  ( ))
sem_Type_TyParen :: (l) ->
                    (Type l) ->
                    (T_Type) (l)
sem_Type_TyParen (loca_ :: (l)) (typ_ :: (Type l)) =
    (let
     in  ( ))
sem_Type_TyInfix :: (l) ->
                    (Type l) ->
                    (MaybePromotedName l) ->
                    (Type l) ->
                    (T_Type) (l)
sem_Type_TyInfix (loca_ :: (l)) (typ1_ :: (Type l)) (maybePromotedName_ :: (MaybePromotedName l)) (typ2_ :: (Type l)) =
    (let
     in  ( ))
sem_Type_TyKind :: (l) ->
                   (Type l) ->
                   (Kind l) ->
                   (T_Type) (l)
sem_Type_TyKind (loca_ :: (l)) (typ_ :: (Type l)) (kind_ :: (Kind l)) =
    (let
     in  ( ))
sem_Type_TyPromoted :: (l) ->
                       (Promoted l) ->
                       (T_Type) (l)
sem_Type_TyPromoted (loca_ :: (l)) (promoted_ :: (Promoted l)) =
    (let
     in  ( ))
sem_Type_TyEquals :: (l) ->
                     (Type l) ->
                     (Type l) ->
                     (T_Type) (l)
sem_Type_TyEquals (loca_ :: (l)) (typ1_ :: (Type l)) (typ2_ :: (Type l)) =
    (let
     in  ( ))
sem_Type_TySplice :: (l) ->
                     (Splice l) ->
                     (T_Type) (l)
sem_Type_TySplice (loca_ :: (l)) (splice_ :: (Splice l)) =
    (let
     in  ( ))
sem_Type_TyBang :: (l) ->
                   (BangType l) ->
                   (Unpackedness l) ->
                   (Type l) ->
                   (T_Type) (l)
sem_Type_TyBang (loca_ :: (l)) (bangType_ :: (BangType l)) (unpackedness_ :: (Unpackedness l)) (typ_ :: (Type l)) =
    (let
     in  ( ))
sem_Type_TyWildCard :: (l) ->
                       (Maybe (Name l)) ->
                       (T_Type) (l)
sem_Type_TyWildCard (loca_ :: (l)) (mName_ :: (Maybe (Name l))) =
    (let
     in  ( ))
sem_Type_TyQuasiQuote :: (l) ->
                         (String) ->
                         (String) ->
                         (T_Type) (l)
sem_Type_TyQuasiQuote (loca_ :: (l)) (str1_ :: (String)) (str2_ :: (String)) =
    (let
     in  ( ))
-- TypeEqn -----------------------------------------------------
-- cata
sem_TypeEqn :: (TypeEqn) (l) ->
               (T_TypeEqn) (l)
sem_TypeEqn (TypeEqn _loca _typeLeft _typeRight) =
    (sem_TypeEqn_TypeEqn _loca _typeLeft _typeRight)
-- semantic domain
type T_TypeEqn l = ( )
data Inh_TypeEqn l = Inh_TypeEqn {}
data Syn_TypeEqn l = Syn_TypeEqn {}
wrap_TypeEqn :: (T_TypeEqn) (l) ->
                (Inh_TypeEqn) (l) ->
                (Syn_TypeEqn) (l)
wrap_TypeEqn sem (Inh_TypeEqn) =
    (let ( ) = sem
     in  (Syn_TypeEqn))
sem_TypeEqn_TypeEqn :: (l) ->
                       (Type l) ->
                       (Type l) ->
                       (T_TypeEqn) (l)
sem_TypeEqn_TypeEqn (loca_ :: (l)) (typeLeft_ :: (Type l)) (typeRight_ :: (Type l)) =
    (let
     in  ( ))
-- Unpackedness ------------------------------------------------
-- cata
sem_Unpackedness :: (Unpackedness) (l) ->
                    (T_Unpackedness) (l)
sem_Unpackedness (Unpack _loca) =
    (sem_Unpackedness_Unpack _loca)
sem_Unpackedness (NoUnpack _loca) =
    (sem_Unpackedness_NoUnpack _loca)
sem_Unpackedness (NoUnpackPragma _loca) =
    (sem_Unpackedness_NoUnpackPragma _loca)
-- semantic domain
type T_Unpackedness l = ( )
data Inh_Unpackedness l = Inh_Unpackedness {}
data Syn_Unpackedness l = Syn_Unpackedness {}
wrap_Unpackedness :: (T_Unpackedness) (l) ->
                     (Inh_Unpackedness) (l) ->
                     (Syn_Unpackedness) (l)
wrap_Unpackedness sem (Inh_Unpackedness) =
    (let ( ) = sem
     in  (Syn_Unpackedness))
sem_Unpackedness_Unpack :: (l) ->
                           (T_Unpackedness) (l)
sem_Unpackedness_Unpack (loca_ :: (l)) =
    (let
     in  ( ))
sem_Unpackedness_NoUnpack :: (l) ->
                             (T_Unpackedness) (l)
sem_Unpackedness_NoUnpack (loca_ :: (l)) =
    (let
     in  ( ))
sem_Unpackedness_NoUnpackPragma :: (l) ->
                                   (T_Unpackedness) (l)
sem_Unpackedness_NoUnpackPragma (loca_ :: (l)) =
    (let
     in  ( ))
-- WarningText -------------------------------------------------
-- cata
sem_WarningText :: (WarningText) (l) ->
                   (T_WarningText) (l)
sem_WarningText (DeprText _loca _string) =
    (sem_WarningText_DeprText _loca _string)
sem_WarningText (WarnText _loca _string) =
    (sem_WarningText_WarnText _loca _string)
-- semantic domain
type T_WarningText l = ( )
data Inh_WarningText l = Inh_WarningText {}
data Syn_WarningText l = Syn_WarningText {}
wrap_WarningText :: (T_WarningText) (l) ->
                    (Inh_WarningText) (l) ->
                    (Syn_WarningText) (l)
wrap_WarningText sem (Inh_WarningText) =
    (let ( ) = sem
     in  (Syn_WarningText))
sem_WarningText_DeprText :: (l) ->
                            (String) ->
                            (T_WarningText) (l)
sem_WarningText_DeprText (loca_ :: (l)) (string_ :: (String)) =
    (let
     in  ( ))
sem_WarningText_WarnText :: (l) ->
                            (String) ->
                            (T_WarningText) (l)
sem_WarningText_WarnText (loca_ :: (l)) (string_ :: (String)) =
    (let
     in  ( ))
-- XAttr -------------------------------------------------------
-- cata
sem_XAttr :: (XAttr) (l) ->
             (T_XAttr) (l)
sem_XAttr (XAttr _loca _xName _exp) =
    (sem_XAttr_XAttr _loca _xName _exp)
-- semantic domain
type T_XAttr l = ( )
data Inh_XAttr l = Inh_XAttr {}
data Syn_XAttr l = Syn_XAttr {}
wrap_XAttr :: (T_XAttr) (l) ->
              (Inh_XAttr) (l) ->
              (Syn_XAttr) (l)
wrap_XAttr sem (Inh_XAttr) =
    (let ( ) = sem
     in  (Syn_XAttr))
sem_XAttr_XAttr :: (l) ->
                   (XName l) ->
                   (Exp l) ->
                   (T_XAttr) (l)
sem_XAttr_XAttr (loca_ :: (l)) (xName_ :: (XName l)) (exp_ :: (Exp l)) =
    (let
     in  ( ))
-- XName -------------------------------------------------------
-- cata
sem_XName :: (XName) (l) ->
             (T_XName) (l)
sem_XName (XName _loca _string) =
    (sem_XName_XName _loca _string)
sem_XName (XDomName _loca _string1 _string2) =
    (sem_XName_XDomName _loca _string1 _string2)
-- semantic domain
type T_XName l = ( )
data Inh_XName l = Inh_XName {}
data Syn_XName l = Syn_XName {}
wrap_XName :: (T_XName) (l) ->
              (Inh_XName) (l) ->
              (Syn_XName) (l)
wrap_XName sem (Inh_XName) =
    (let ( ) = sem
     in  (Syn_XName))
sem_XName_XName :: (l) ->
                   (String) ->
                   (T_XName) (l)
sem_XName_XName (loca_ :: (l)) (string_ :: (String)) =
    (let
     in  ( ))
sem_XName_XDomName :: (l) ->
                      (String) ->
                      (String) ->
                      (T_XName) (l)
sem_XName_XDomName (loca_ :: (l)) (string1_ :: (String)) (string2_ :: (String)) =
    (let
     in  ( ))
