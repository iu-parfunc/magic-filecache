
-- | 

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Control.DeepSeq
import Language.Haskell.Exts.Syntax
import GHC.Generics

deriving instance Generic Type
deriving instance NFData Type
-- Needed for Decl:
deriving instance NFData ModuleName
deriving instance NFData Decl
deriving instance NFData QualConDecl
deriving instance NFData Rhs
deriving instance NFData GuardedRhs
deriving instance NFData QualStmt
deriving instance NFData XAttr
deriving instance NFData PXAttr
deriving instance NFData Stmt
deriving instance NFData Splice
deriving instance NFData Bracket
deriving instance NFData ConDecl
deriving instance NFData Alt
deriving instance NFData Asst
deriving instance NFData PatternSynDirection
deriving instance NFData Overlap
deriving instance NFData Op
deriving instance NFData Match
deriving instance NFData GadtDecl
deriving instance NFData FunDep
deriving instance NFData DataOrNew
deriving instance NFData ClassDecl
deriving instance NFData CallConv
deriving instance NFData BooleanFormula
deriving instance NFData Assoc
deriving instance NFData Annotation
deriving instance NFData InstDecl
deriving instance NFData TypeEqn
deriving instance NFData SrcLoc
deriving instance NFData Safety
deriving instance NFData TyVarBind
deriving instance NFData Name
deriving instance NFData Rule
deriving instance NFData RuleVar
deriving instance NFData Kind
deriving instance NFData Role
deriving instance NFData Exp
deriving instance NFData QName
deriving instance NFData QOp
deriving instance NFData Pat
deriving instance NFData PatField
deriving instance NFData FieldUpdate
deriving instance NFData RPat
deriving instance NFData Promoted
deriving instance NFData BangType
deriving instance NFData RPatOp
deriving instance NFData IPBind
deriving instance NFData IPName
deriving instance NFData XName
deriving instance NFData Sign
deriving instance NFData Literal
deriving instance NFData Binds
deriving instance NFData Boxed
deriving instance NFData Activation
deriving instance NFData SpecialCon
-- Additionally needed for Module:
deriving instance NFData Module
deriving instance NFData WarningText
deriving instance NFData ModulePragma
deriving instance NFData ImportDecl
deriving instance NFData Tool
deriving instance NFData ExportSpec
deriving instance NFData ImportSpec
deriving instance NFData Namespace
deriving instance NFData CName

main = print "hello"
