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
   = DecVar DecVarContent
   | DecFunc DecFuncContent
   | DecClass DecClassContent
   | DecMethod DecMethodContent
   | DecImport DecImportContent
   deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data Exp
   = ExpInt ExpIntContent
   | ExpStr ExpStrContent
   | ExpVar ExpVarContent
   | ExpCall ExpCallContent
   | ExpBinop ExpBinopContent
   deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data Stmt
   = StmtTry StmtTry
   | StmtCall StmtCallContent
   | StmtBreak StmtBreakContent
   | StmtWhile StmtWhileContent
   | StmtDecvar StmtDecvarContent
   | StmtAssign StmtAssignContent
   | StmtReturn StmtReturnContent
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

data DecMethodContent
   = DecMethodContent
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
         actualMethods :: Map Token.MethdName DecMethodContent
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data DecClassContent
   = DecClassContent
     {
         decClassName :: Token.ClassName,
         decClassSupers :: [ Token.SuperName ],
         decClassDataMembers :: DataMembers,
         decClassMethods :: Methods
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data DecFuncContent
   = DecFuncContent
     {
         decFuncReturnType :: Token.NominalTy,
         decFuncName       :: Token.FuncName,
         decFuncParams     :: [ Param ],
         decFuncBody       :: [ Stmt ]
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data DecVarContent
   = DecVarContent
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

data DecImportContent
   = DecImportLocalCtor ImportLocal 
   | DecImportNonLocalCtor ImportNonLocal
   deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data ExpIntContent
   = ExpIntContent
     {
         expIntValue :: Token.ConstInt
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data ExpStrContent
   = ExpStrContent
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

data ExpBinopContent
   = ExpBinopContent
     {
         operand0 :: Exp,
         operand1 :: Exp,
         operator :: Operator
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data ExpVarContent
   = ExpVarContent
     {
         actualExpVar :: Var
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data StmtAssignContent
   = StmtAssignContent
     {
         stmtAssignLhs :: Var,
         stmtAssignRhs :: Exp
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data StmtTryContent
   = StmtTryContent
     {
         stmtTryPart :: [ Stmt ],
         stmtCatchPart :: [ Stmt ]
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data StmtBreakContent
   = StmtBreakContent
     {
         stmtBreakLocation :: Location
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data StmtWhileContent
   = StmtWhileContent
     {
         stmtWhileCond :: Exp,
         stmtWhileBody :: [ Stmt ]
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data StmtReturnContent
   = StmtReturnContent
     {
         stmtReturnValue :: Maybe Exp
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data StmtCallContent
   = StmtCallContent
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
