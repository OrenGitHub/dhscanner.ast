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
         decs :: [ Dec ],
         stmts :: [ Stmt ]
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data Dec
   = DecVar DecVarContent
   | DecFunc DecFuncContent
   | DecClass DecClassContent
   | DecMethod DecMethodContent
   deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data Exp
   = ExpInt ExpIntContent
   | ExpStr ExpStrContent
   | ExpVar ExpVarContent
   | ExpBool ExpBoolContent
   | ExpCall ExpCallContent
   | ExpBinop ExpBinopContent
   | ExpLambda ExpLambdaContent
   deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data Stmt
   = StmtIf StmtIfContent
   | StmtTry StmtTryContent
   | StmtCall ExpCallContent
   | StmtDecvar DecVarContent
   | StmtBreak StmtBreakContent
   | StmtWhile StmtWhileContent
   | StmtImport StmtImportContent
   | StmtAssign StmtAssignContent
   | StmtReturn StmtReturnContent
   | StmtContinue StmtContinueContent
   deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data Param
   = Param
     {
         paramName :: Token.ParamName,
         paramNominalType :: Token.NominalTy,
         paramSerialIdx :: Word
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data DataMember
   = DataMember
     {
         dataMemberName :: Token.MembrName,
         dataMemberNominalType :: Token.NominalTy,
         dataMemberInitValue :: Maybe Exp
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data DataMembers
   = DataMembers
     {
         actualDataMembers :: Map Token.MembrName DataMember
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data DecMethodContent
   = DecMethodContent
     {
         decMethodReturnType :: Token.NominalTy,
         decMethodName :: Token.MethdName,
         decMethodParams :: [ Param ],
         decMethodBody :: [ Stmt ]
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data Methods
   = Methods
     {
         actualMethods :: Map Token.MethdName DecMethodContent
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data DecClassContent
   = DecClassContent
     {
         decClassName :: Token.ClassName,
         decClassSupers :: [ Token.SuperName ],
         decClassDataMembers :: DataMembers,
         decClassMethods :: Methods
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data DecFuncContent
   = DecFuncContent
     {
         decFuncReturnType :: Token.NominalTy,
         decFuncName :: Token.FuncName,
         decFuncParams :: [ Param ],
         decFuncBody :: [ Stmt ]
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data DecPackageContent
   = DecPackageContent
     {
         decPackageName :: Token.PkgName
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data DecVarContent
   = DecVarContent
     {
         decVarName :: Token.VarName,
         decVarNominalType :: Token.NominalTy,
         decVarInitValue :: Maybe Exp
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data ExpIntContent
   = ExpIntContent
     {
         expIntValue :: Token.ConstInt
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data ExpStrContent
   = ExpStrContent
     {
         expStrValue :: Token.ConstStr
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data ExpBoolContent
   = ExpBoolContent
     {
         expBoolValue :: Token.ConstBool
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data Operator
   = PLUS
   | MINUS
   | TIMES
   | DIVIDE
   deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data ExpLambdaContent
   = ExpLambdaContent
     {
         expLambdaParams :: [ Param ],
         expLambdaBody :: [ Stmt ],
         expLambdaLocation :: Location
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data ExpBinopContent
   = ExpBinopContent
     {
         expBinopLeft :: Exp,
         expBinopRight :: Exp,
         expBinopOperator :: Operator,
         expBinopLocation :: Location
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data ExpVarContent
   = ExpVarContent
     {
         actualExpVar :: Var
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data StmtAssignContent
   = StmtAssignContent
     {
         stmtAssignLhs :: Var,
         stmtAssignRhs :: Exp
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data StmtTryContent
   = StmtTryContent
     {
         stmtTryPart :: [ Stmt ],
         stmtCatchPart :: [ Stmt ],
         stmtTryLocation :: Location
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data StmtBreakContent
   = StmtBreakContent
     {
         stmtBreakLocation :: Location
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data StmtImportContent
   = StmtImportContent
     {
         stmtImportLocation :: Location
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data StmtContinueContent
   = StmtContinueContent
     {
         stmtContinueLocation :: Location
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data StmtIfContent
   = StmtIfContent
     {
         stmtIfCond :: Exp,
         stmtIfBody :: [ Stmt ],
         stmtIfLocation :: Location
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data StmtWhileContent
   = StmtWhileContent
     {
         stmtWhileCond :: Exp,
         stmtWhileBody :: [ Stmt ],
         stmtWhileLocation :: Location
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data StmtReturnContent
   = StmtReturnContent
     {
         stmtReturnValue :: Maybe Exp,
         stmtReturnLocation :: Location
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data ExpCallContent
   = ExpCallContent
     {
         callee :: Exp,
         args :: [ Exp ],
         expCallLocation :: Location
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data VarFieldContent
   = VarFieldContent
     {
         varFieldLhs :: ExpVarContent,
         varFieldName :: Token.FieldName,
         varFieldLocation :: Location
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data VarSimpleContent
   = VarSimpleContent
     {
         varName :: Token.VarName
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data VarSubscriptContent
   = VarSubscriptContent
     {
         varSubscriptLhs :: Exp,
         varSubscriptIdx :: Exp,
         varSubscriptLocation :: Location
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data Var
   = VarSimple VarSimpleContent
   | VarField VarFieldContent
   | VarSubscript VarSubscriptContent
   deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

locationVar :: Var -> Location
locationVar (VarSimple    v) = Token.getVarNameLocation (varName v)
locationVar (VarField     v) = varFieldLocation v
locationVar (VarSubscript v) = varSubscriptLocation v

