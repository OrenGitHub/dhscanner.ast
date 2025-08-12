-- |
--
-- * The [abstract ayntax tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree) ( ast )
--   aims to be a data structure able to:
--
--     * represent /multiple/ ( native ) ast kinds
--     * from /various/ programming languages
--
-- * Its main purpose is to serve as the:
--
--     * first step for /static code analysis/ 
--     * part of the [dhscanner](https://github.com/OrenGitHub/dhscanner) framework for
--       CI\/CD container security checks 🔒 and
--       [PII](https://en.wikipedia.org/wiki/Personal_data) leaks detection 🪪
--
-- * As part of the [dhscanner](https://github.com/OrenGitHub/dhscanner) framework:
--
--     * targets mostly languages used for /cloud native applications/ ☁️
--     * Python, Ruby 💎, Php, Javascript, Typescript, Java ☕️, C# and Golang.
--
-- * Typical flow:
--
--     * a file is parsed with the corresponding native parser of the language it's written in
--
--         * see [Python's native parser](https://docs.python.org/3/library/ast.html), for example
--         * native parsers hosted on independent micro services
--
--     * the native ast is dumped (as JSON, or plain text)
--
--     * dumped content is sent to a [Happy](https://haskell-happy.readthedocs.io/en/latest/) +
--       [Alex](https://haskell-alex.readthedocs.io/en/latest/) Haskell parser
--
--     * the Haskell parser organizes the natively parsed content into an ast
--
-- * Geared towards static code analysis, the ast design abstracts away details that are normally ignored anyway
--
--     * for example, it does not distinguish between `try` and `catch` blocks
--
--     * it models both of them as plain sequential code blocks.
--
-- * Every file has exactly one ast ( 'Root' ) that represents it
--
-- * Non Haskell parogrammers note:
--
--     * The ast is /immutable/ ( like everything else in Haskell ... )
--

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

-- |
-- * every file has /exactly one/ root 🌱
--
-- * classes, functions and methods are organized as /statements/
--
data Root
   = Root
     {
         stmts :: [ Stmt ]
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data Exp
   = ExpInt ExpIntContent
   | ExpStr ExpStrContent
   | ExpVar ExpVarContent
   | ExpBool ExpBoolContent
   | ExpNull ExpNullContent
   | ExpCall ExpCallContent
   | ExpBinop ExpBinopContent
   | ExpAssign ExpAssignContent
   | ExpLambda ExpLambdaContent
   deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data Stmt
   = StmtExp Exp
   | StmtIf StmtIfContent
   | StmtTry StmtTryContent
   | StmtFunc StmtFuncContent
   | StmtBlock StmtBlockContent
   | StmtBreak StmtBreakContent
   | StmtClass StmtClassContent
   | StmtWhile StmtWhileContent
   | StmtImport StmtImportContent
   | StmtMethod StmtMethodContent
   | StmtAssign StmtAssignContent
   | StmtReturn StmtReturnContent
   | StmtVardec StmtVardecContent
   | StmtContinue StmtContinueContent
   deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

-- |
--
-- * used by:
--
--       * 'StmtFuncContent'
--       * 'StmtMethodContent'
--       * 'ExpLambdaContent'
--
-- * note:
--
--       * `self` param in python methods has 'paramSerialIdx' = 0
--       * `self` param in python methods is /ignored/ when assigning the serial index to /other/ params
--       * see the example code below
--
-- ==== __Example:__
--
-- * params enumeration for python methods
--
-- @
-- class Person:
--     # paramSerialIdx = 1 -----------------------+
--     # paramSerialIdx = 0 ------------+          |
--     # paramSerialIdx = 0 ------+     |          |
--     #                          |     |          |
--     def some_arbitrary_method(self, name: str, age: int) -> None:
--         pass
-- @
data Param
   = Param
     {
         paramName :: Token.ParamName,
         paramNominalType :: Maybe Var,
         paramSerialIdx :: Word -- ^ ( /zero/-based, ignores `self` in python methods when enumerating /other/ params )
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data DataMember
   = DataMember
     {
         dataMemberName :: Token.MemberName,
         dataMemberNominalType :: Maybe Var,
         dataMemberInitValue :: Maybe Exp
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data DataMembers
   = DataMembers
     {
         actualDataMembers :: Map Token.MemberName DataMember
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data StmtMethodContent
   = StmtMethodContent
     {
         stmtMethodReturnType :: Maybe Var,
         stmtMethodName :: Token.MethodName,
         stmtMethodParams :: [ Param ],
         stmtMethodBody :: [ Stmt ],
         stmtMethodLocation :: Location,
         hostingClassName :: Token.ClassName,
         hostingClassSupers :: [ Token.SuperName ]
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data Methods
   = Methods
     {
         actualMethods :: Map Token.MethodName StmtMethodContent
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data StmtClassContent
   = StmtClassContent
     {
         stmtClassName :: Token.ClassName,
         stmtClassSupers :: [ Token.SuperName ],
         stmtClassDataMembers :: DataMembers,
         stmtClassMethods :: Methods
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data StmtFuncContent
   = StmtFuncContent
     {
         stmtFuncReturnType :: Maybe Var,
         stmtFuncName :: Token.FuncName,
         stmtFuncParams :: [ Param ],
         stmtFuncBody :: [ Stmt ],
         stmtFuncAnnotations :: [ Exp ],
         stmtFuncLocation :: Location
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data StmtVardecContent
   = StmtVardecContent
     {
         stmtVardecName :: Token.VarName,
         stmtVardecNominalType :: Maybe Var,
         stmtVardecInitValue :: Maybe Exp,
         stmtVardecLocation :: Location
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

data ExpNullContent
   = ExpNullContent
     {
         expNullValue :: Token.ConstNull
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data Operator
   = PLUS
   | MINUS
   | TIMES
   | DIVIDE
   | PERCENT
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

data ExpAssignContent
   = ExpAssignContent
     {
         expAssignLhs :: Var,
         expAssignRhs :: Exp,
         expAssignLocation :: Location
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

data StmtBlockContent
   = StmtBlockContent
     {
         stmtBlockContent :: [ Stmt ],
         stmtBlockLocation :: Location
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

-- |
-- ==== __Examples:__
-- 
-- * Simple source import
--
-- @
-- # stmtImportSource is "json" 
-- # stmtImportFromSource is Nothing 
-- # stmtImportAlias is Nothing
-- import json
-- @
--
-- * Specifying a specific name from source
--
-- @
-- # stmtImportSource is "urllib.parse"
-- # stmtImportFromSource is Just "urljoin"
-- # stmtImportAlias is Nothing
-- from urllib.parse import urljoin
-- @
--
-- * Specifying an alias for a source import
--
-- @
-- # stmtImportSource is "networkx"
-- # stmtImportFromSource is Nothing
-- # stmtImportAlias is Just "nx"
-- import networkx as nx
-- @
--
data StmtImportContent
   = StmtImportContent
     {
         stmtImportSource :: String,
         stmtImportFromSource :: Maybe String,
         stmtImportAlias :: Maybe String,
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
         stmtElseBody :: [ Stmt ],
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
         varFieldLhs :: Exp,
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
