-- |
-- * [Abstract Syntax Tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree)
--
-- * can represent /multiple/ programming languages
--
-- * /Immutable/
--
-- * Suuports (de)serialization to(from) JSON
--
-- * One AST per one source file
--
-- * /Every/ AST node has an associated location (not just tokens)

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Ast

where

import qualified Token

import Data.Aeson
import GHC.Generics
import Data.Map ( Map )

data Root
   = Root
     {
         filename :: String,
         rootContent :: [ Dec ]
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data Asts
   = Asts
     {
         dirname :: String,
         astsContent :: [ Root ]
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data Dec
   = DecVar DecVarContent
   | DecFunc DecFuncContent
   | DecClass DecClassContent
   | DecScriptAssign StmtAssignContent
   deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data Exp
   = ExpInt Token.ConstInt
   | ExpStr Token.ConstStr
   | ExpCall ExpCallContent
   | ExpBinop ExpBinopContent
   | ExpVar Var
   deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data Stmt
   = StmtWhile StmtWhileContent
   | StmtAssign StmtAssignContent
   | StmtReturn (Maybe Exp)
   | StmtDecvar DecVarContent
   | StmtCall ExpCallContent
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
         dataMembersContent :: Map Token.MembrName DataMember
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
         methodsContent :: Map Token.MethdName DecMethodContent
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

data ExpBinopContent
   = ExpBinopPlus   Exp Exp
   | ExpBinopMinus  Exp Exp
   | ExpBinopTimes  Exp Exp
   | ExpBinopDivide Exp Exp
   deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data StmtAssignContent
   = StmtAssignContent
     {
         stmtAssignLhs :: Var,
         stmtAssignRhs :: Exp
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data StmtWhileContent
   = StmtWhileContent
     {
         stmtWhileCond :: Exp,
         stmtWhileBody :: [Stmt]
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data ExpCallContent
   = ExpCallContent
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

