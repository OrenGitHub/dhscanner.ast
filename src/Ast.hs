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
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data Dec
   = DecVar DecVarContent
   | DecFunc DecFuncContent
   | DecClass DecClassContent
   | DecMethod DecMethodContent
   | DecImport DecImportContent
   | DecPackage DecPackageContent
   deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data Exp
   = ExpInt ExpIntContent
   | ExpStr ExpStrContent
   | ExpVar ExpVarContent
   | ExpCall ExpCallContent
   | ExpBinop ExpBinopContent
   deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data Stmt
   = StmtTry StmtTryContent
   | StmtCall ExpCallContent
   | StmtDecvar DecVarContent
   | StmtBreak StmtBreakContent
   | StmtWhile StmtWhileContent
   | StmtAssign StmtAssignContent
   | StmtReturn StmtReturnContent
   | StmtContinue StmtContinueContent
   deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data Param
   = Param
     {
         paramName        :: Token.ParamName,
         paramNominalType :: Token.NominalTy,
         paramSerialIdx   :: Word
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
         decMethodName       :: Token.MethdName,
         decMethodParams     :: [ Param ],
         decMethodBody       :: [ Stmt ]
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
         decFuncName       :: Token.FuncName,
         decFuncParams     :: [ Param ],
         decFuncBody       :: [ Stmt ]
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data DecPackageContent
   = DecPackageContent
     {
         decPackageName :: Token.PkgName
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

decFuncLocation :: DecFuncContent -> Location
decFuncLocation = Token.getFuncNameLocation . decFuncName

data DecVarContent
   = DecVarContent
     {
         decVarName        :: Token.VarName,
         decVarNominalType :: Token.NominalTy,
         decVarInitValue   :: Maybe Exp
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data ImportLocalWildcard
   = ImportLocalWildcard
     {
         importLocalWildcardPackage :: [ Token.Named ],
         importLocalWildcardLevel :: Int,
         importLocalWildcardLocation :: Location
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data ImportLocalNormal
   = ImportLocalNormal
     {
         importLocalNormalPackage :: [ Token.Named ],
         importLocalNormalLevel :: Int,
         importLocalNormalNames :: [ Token.Named ],
         importLocalNormalLocation :: Location
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data ImportNonLocalWildcard
   = ImportNonLocalWildcard
     {
         importNonLocalWildcardPackage :: [ Token.Named ],
         importNonLocalWildcardLocation :: Location
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data ImportNonLocalNormal
   = ImportNonLocalNormal
     {
         importNonLocalNormalPackage :: [ Token.Named ],
         importNonLocalNormalNames :: [[ Token.Named ]],
         importNonLocalNormalLocation :: Location
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data ImportLocal
   = ImportLocalWildcardCtor ImportLocalWildcard
   | ImportLocalNormalCtor ImportLocalNormal
   deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data ImportNonLocal
   = ImportNonLocalWildcardCtor ImportNonLocalWildcard
   | ImportNonLocalNormalCtor ImportNonLocalNormal
   deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data DecImportContent
   = DecImportLocalCtor ImportLocal 
   | DecImportNonLocalCtor ImportNonLocal
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

data Operator
   = PLUS
   | MINUS
   | TIMES
   | DIVIDE
   deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data ExpBinopContent
   = ExpBinopContent
     {
         operand0 :: Exp,
         operand1 :: Exp,
         operator :: Operator
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

data StmtContinueContent
   = StmtContinueContent
     {
         stmtContinueLocation :: Location
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
         args :: [ Exp ]
     }
     deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )

data Var
   = VarSimple Token.VarName
   | VarField Exp Token.FieldName
   | VarSubscript Exp Exp
   deriving ( Show, Eq, Ord, Generic, ToJSON, FromJSON )


-- -------------------- locations -------------------- 

locationDec :: Dec -> Location
-- ^ don't keep explicit location inside declarations.
-- instead, assemble the location only when it's needed.
locationDec (DecVar     d) = locationDecVar     d
locationDec (DecFunc    d) = locationDecFunc    d
locationDec (DecClass   d) = locationDecClass   d
locationDec (DecMethod  d) = locationDecMethod  d
locationDec (DecImport  d) = locationDecImport  d
locationDec (DecPackage d) = locationDecPackage d

locationExp :: Exp -> Location
-- ^ don't keep explicit location inside expressions.
-- instead, assemble the location only when it's needed.
locationExp (ExpInt   e) = locationExpInt   e
locationExp (ExpStr   e) = locationExpStr   e
locationExp (ExpVar   e) = locationExpVar   e
locationExp (ExpCall  e) = locationExpCall  e
locationExp (ExpBinop e) = locationExpBinop e

locationStmt :: Stmt -> Location
-- ^ don't keep explicit location inside statements.
-- instead, assemble the location only when it's needed.
locationStmt (StmtTry      s) = locationStmtTry      s
locationStmt (StmtCall     s) = locationStmtCall     s
locationStmt (StmtDecvar   s) = locationStmtDecvar   s
locationStmt (StmtBreak    s) = locationStmtBreak    s
locationStmt (StmtWhile    s) = locationStmtWhile    s
locationStmt (StmtAssign   s) = locationStmtAssign   s
locationStmt (StmtReturn   s) = locationStmtReturn   s
locationStmt (StmtContinue s) = locationStmtContinue s

-- -------------------- locations -------------------- 

locationDecVar     :: DecVarContent     -> Location
locationDecFunc    :: DecFuncContent    -> Location
locationDecClass   :: DecClassContent   -> Location
locationDecMethod  :: DecMethodContent  -> Location
locationDecImport  :: DecImportContent  -> Location
locationDecPackage :: DecPackageContent -> Location

-- -------------------- locations -------------------- 

locationDecVar     = Token.getVarNameLocation . decVarName
locationDecFunc    = Token.getFuncNameLocation . decFuncName
locationDecClass   = Token.getClassNameLocation . decClassName
locationDecMethod  = Token.getMethodNameLocation . decMethodName

-- -------------------- locations -------------------- 

locationDecImport (DecImportLocalCtor    (ImportLocalWildcardCtor    i)) = importLocalWildcardLocation    i
locationDecImport (DecImportLocalCtor    (ImportLocalNormalCtor      i)) = importLocalNormalLocation      i
locationDecImport (DecImportNonLocalCtor (ImportNonLocalWildcardCtor i)) = importNonLocalWildcardLocation i
locationDecImport (DecImportNonLocalCtor (ImportNonLocalNormalCtor   i)) = importNonLocalNormalLocation   i

locationDecPackage = undefined

-- -------------------- locations -------------------- 

locationExpInt   :: ExpIntContent   -> Location
locationExpStr   :: ExpStrContent   -> Location
locationExpVar   :: ExpVarContent   -> Location
locationExpCall  :: ExpCallContent  -> Location
locationExpBinop :: ExpBinopContent -> Location

-- -------------------- locations -------------------- 

locationExpInt   = Token.constIntLocation . expIntValue
locationExpStr   = Token.constStrLocation . expStrValue
locationExpVar   = locationVar . actualExpVar 
locationExpCall  = locationExp . callee
locationExpBinop = locationExp . operand0

-- -------------------- locations -------------------- 

locationStmtTry      :: StmtTryContent      -> Location
locationStmtCall     :: ExpCallContent      -> Location
locationStmtDecvar   :: DecVarContent       -> Location
locationStmtBreak    :: StmtBreakContent    -> Location
locationStmtWhile    :: StmtWhileContent    -> Location
locationStmtAssign   :: StmtAssignContent   -> Location
locationStmtReturn   :: StmtReturnContent   -> Location
locationStmtContinue :: StmtContinueContent -> Location

-- -------------------- locations -------------------- 

locationStmtTry      = stmtTryLocation 
locationStmtCall     = locationExp . callee
locationStmtDecvar   = Token.getVarNameLocation . decVarName
locationStmtBreak    = stmtBreakLocation
locationStmtWhile    = stmtWhileLocation
locationStmtAssign   = locationVar . stmtAssignLhs
locationStmtReturn   = stmtReturnLocation
locationStmtContinue = stmtContinueLocation

-- -------------------- locations -------------------- 

locationVar :: Var -> Location
locationVar (VarSimple      t) = Token.getVarNameLocation t
locationVar (VarField     _ f) = Token.getFieldNameLocation f
locationVar (VarSubscript v _) = locationExp v 

