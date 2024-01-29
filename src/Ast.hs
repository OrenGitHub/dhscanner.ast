-- |
-- * [Abstract Syntax Tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree)
--
-- * can represent /multiple/ programming languages
--
-- * /Immutable/
--
-- * Supports (de)serialization to(from) JSON
--
-- * One AST per one source file
--
-- * /Every/ AST node has an associated location (not just tokens)
--
-- * Meant to be used with `qualified import`

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Ast

where

import Data.Aeson
import GHC.Generics
import Data.Map ( Map )

-- project imports
import Location
import qualified Token

data Root
   = Root
     {
         filename :: String,
         actualAst :: [ Dec ]
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data Dec
   = DecVarCtor DecVar
   | DecFuncCtor DecFunc
   | DecClassCtor DecClass
   | DecImportCtor DecImport
   deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data Exp
   = ExpIntCtor ExpInt
   | ExpStrCtor ExpStr
   | ExpVarCtor ExpVar
   | ExpCallCtor ExpCall
   | ExpBinopCtor ExpBinop
   deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data Stmt
   = StmtTryCtor StmtTry
   | StmtCallCtor ExpCall
   | StmtDecvarCtor DecVar
   | StmtBreakCtor StmtBreak
   | StmtWhileCtor StmtWhile
   | StmtAssignCtor StmtAssign
   | StmtReturnCtor StmtReturn
   deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data Param
   = Param
     {
         paramName        :: Token.ParamName,
         paramNominalType :: Token.NominalTy,
         paramSerialIdx   :: Word
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data DataMember
   = DataMember
     {
         dataMemberName :: Token.MembrName,
         dataMemberNominalType :: Token.NominalTy,
         dataMemberInitValue :: Maybe Exp
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data DataMembers
   = DataMembers
     {
         actualDataMembers :: Map Token.MembrName DataMember
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data DecMethod
   = DecMethod
     {
         decMethodReturnType :: Token.NominalTy,
         decMethodName       :: Token.MethdName,
         decMethodParams     :: [ Param ],
         decMethodBody       :: [ Stmt ]
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data Methods
   = Methods
     {
         actualMethods :: Map Token.MethdName DecMethod
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data DecClass
   = DecClass
     {
         decClassName :: Token.ClassName,
         decClassSupers :: [ Token.SuperName ],
         decClassDataMembers :: DataMembers,
         decClassMethods :: Methods
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data DecFunc
   = DecFunc
     {
         decFuncReturnType :: Token.NominalTy,
         decFuncName       :: Token.FuncName,
         decFuncParams     :: [ Param ],
         decFuncBody       :: [ Stmt ]
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data DecVar
   = DecVar
     {
         decVarName        :: Token.VarName,
         decVarNominalType :: Token.NominalTy,
         decVarInitValue   :: Maybe Exp
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data ImportLocalWildcard
   = ImportLocalWildcard
     {
         importLocalWildcardPackage :: [ Token.Named ],
         importLocalWildcardLevel :: Int
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data ImportLocalNormal
   = ImportLocalNormal
     {
         importLocalNormalPackage :: [ Token.Named ],
         importLocalNormalLevel :: Int,
         importLocalNormalNames :: [ Token.Named ]
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data ImportNonLocalWildcard
   = ImportNonLocalWildcard
     {
         importNonLocalWildcardPackage :: [ Token.Named ]
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data ImportNonLocalNormal
   = ImportNonLocalNormal
     {
         importNonLocalNormalPackage :: [ Token.Named ],
         importNonLocalNormalNames :: [[ Token.Named ]]
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data ImportLocal
   = ImportLocalWildcardCtor ImportLocalWildcard
   | ImportLocalNormalCtor ImportLocalNormal
   deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data ImportNonLocal
   = ImportNonLocalWildcardCtor ImportNonLocalWildcard
   | ImportNonLocalNormalCtor ImportNonLocalNormal
   deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data DecImport
   = DecImportLocalCtor ImportLocal 
   | DecImportNonLocalCtor ImportNonLocal
   deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data ExpInt
   = ExpInt
     {
         expIntValue :: Token.ConstInt
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data ExpStr
   = ExpStr
     {
         expStrValue :: Token.ConstStr
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data Operator
   = PLUS
   | MINUS
   | TIMES
   | DIVIDE
   deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data ExpBinop
   = ExpBinop
     {
         operand0 :: Exp,
         operand1 :: Exp,
         operator :: Operator
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data ExpVar
   = ExpVar
     {
         actualExpVar :: Var
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data StmtAssign
   = StmtAssign
     {
         stmtAssignLhs :: Var,
         stmtAssignRhs :: Exp
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data StmtTry
   = StmtTry
     {
         stmtTryPart :: [ Stmt ],
         stmtCatchPart :: [ Stmt ]
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data StmtBreak
   = StmtBreak
     {
         stmtBreakLocation :: Location
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data StmtWhile
   = StmtWhile
     {
         stmtWhileCond :: Exp,
         stmtWhileBody :: [ Stmt ]
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data StmtReturn
   = StmtReturn
     {
         stmtReturnValue :: Maybe Exp
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data ExpCall
   = ExpCall
     {
         callee :: Exp,
         args :: [ Exp ]
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data Var
   = VarSimple Token.VarName
   | VarField Exp Token.FieldName
   | VarSubscript Exp Exp
   deriving ( Show, Eq, Generic, ToJSON, FromJSON )

