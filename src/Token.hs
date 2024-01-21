{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Token

where

import Location

import Data.Aeson
import GHC.Generics

data Token
   = Token
   {
       content :: String,
       location :: Location
   }
   deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )
   
data VarName   = VarName   Token deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )
data FuncName  = FuncName  Token deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )
data ParamName = ParamName Token deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )
data FieldName = FieldName Token deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )
data ClassName = ClassName Token deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )
data SuperName = SuperName Token deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )
data MembrName = MembrName Token deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord, ToJSONKey, FromJSONKey )
data MethdName = MethdName Token deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord, ToJSONKey, FromJSONKey )
data NominalTy = NominalTy Token deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

-- signatures (to quiet ghc -Wall)
getVarNameToken   :: VarName   -> Token
getFuncNameToken  :: FuncName  -> Token
getParamNameToken :: ParamName -> Token
getFieldNameToken :: FieldName -> Token
getClassNameToken :: ClassName -> Token
getSuperNameToken :: SuperName -> Token
getMembrNameToken :: MembrName -> Token
getMethdNameToken :: MethdName -> Token
getNominalTyToken :: NominalTy -> Token

-- getters
getVarNameToken   (VarName   v) = v
getFuncNameToken  (FuncName  f) = f
getParamNameToken (ParamName p) = p
getFieldNameToken (FieldName f) = f
getClassNameToken (ClassName c) = c
getSuperNameToken (SuperName s) = s
getMembrNameToken (MembrName m) = m
getMethdNameToken (MethdName m) = m
getNominalTyToken (NominalTy t) = t

-- locations
getSrcVariableLocation :: VarName -> Location
getSrcVariableLocation = location . getVarNameToken

data ConstInt
   = ConstInt
   {
       constIntValue :: Int,
       constIntLocation :: Location
   }
   deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

data ConstStr
   = ConstStr
   {
       constStrValue :: String,
       constStrLocation :: Location
   }
   deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )
