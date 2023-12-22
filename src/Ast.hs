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

data Root
   = Root
     {
         filename: String,
         rootContent: [ Dec ]
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data Asts
   = Asts
     {
         dirname: String,
         astsContent: [ Root ]
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data Dec
   = DecVar DecVarContent
   | DecFunc DecFuncContent
   deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data Exp
   = ExpInt Token.ConstInt
   | ExpCall ExpCallContent
   | ExpBinop ExpBinopContent
   | ExpVar Var
   deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data Stmt
   = StmtWhile StmtWhileContent
   | StmtAssign Var Exp
   | StmtReturn (Maybe Exp)
   deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data Param
   = Param
     {
         paramName        :: Token.ParamName,
         paramNominalType :: Token.NominalTy,
         paramSerialIdx   :: Word
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
   | VarField Var Token.FieldName
   | VarSubscript Var Exp
   deriving ( Show, Eq, Generic, ToJSON, FromJSON )

