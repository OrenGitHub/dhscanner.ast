-- |
--
-- * The [abstract ayntax tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree) (ast)
--   aims to be a data structure able to represent /multiple/ abstract syntax trees from
--   /various/ programming languages.
--
-- * Its main purpose is to serve as the first step for /static code analysis/,
--   as part of the [dhscanner](https://github.com/OrenGitHub/dhscanner) framework
--   for CI/CD container security checks.
--
-- * As part of that framework, it targets mostly languages used for /cloud native applications/:
--   __Python__, __Ruby__, __Php__, __Javascript__, __Typescript__, __Java__ and __Golang__.
--
-- * Typically, a file is first parsed with the corresponding native parser
--   of the language it's written in
--   (see [Python's native parser](https://docs.python.org/3/library/ast.html) for example).
--   The native ast is then dumped (as JSON, or plain text)
--   and sent to a [Happy](https://haskell-happy.readthedocs.io/en/latest/) +
--   [Alex](https://haskell-alex.readthedocs.io/en/latest/) Haskell parser
--   which accommodates the natively parsed content into the ast.
--
-- * Geared towards static code analysis, the ast design abstracts away details
--   that are normally ignored anyway. For example, it does not distinguish between
--   __try__ and __catch__ blocks, and models both of them as plain sequential code blocks.
--
-- * Every file has exactly one ast that represents it.
--
-- * Non Haskell parogrammers note: The ast is /immutable/ (like everything else in Haskell ...)
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
   = StmtExp Exp
   | StmtIf StmtIfContent
   | StmtTry StmtTryContent
   | StmtCall ExpCallContent
   | StmtFunc StmtFuncContent
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
         decMethodBody :: [ Stmt ],
         decMethodLocation :: Location,
         hostingClassName :: Token.ClassName,
         hostingClassSupers :: [ Token.SuperName ]
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

data StmtFuncContent
   = StmtFuncContent
     {
         stmtFuncReturnType :: Token.NominalTy,
         stmtFuncName :: Token.FuncName,
         stmtFuncParams :: [ Param ],
         stmtFuncBody :: [ Stmt ],
         stmtFuncAnnotations :: [ Exp ],
         stmtFuncLocation :: Location
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
         stmtImportName :: String,
         stmtImportAlias :: String,
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

locationVar :: Var -> Location
locationVar (VarSimple    v) = Token.getVarNameLocation (varName v)
locationVar (VarField     v) = varFieldLocation v
locationVar (VarSubscript v) = varSubscriptLocation v
