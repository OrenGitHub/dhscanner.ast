{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Token

where

import Location

import Data.Aeson
import GHC.Generics

data Named
   = Named
   {
       content :: String,
       location :: Location
   }
   deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )
   
data VarName   = VarName   Named deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )
data PkgName   = PkgName   Named deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )
data FuncName  = FuncName  Named deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )
data ParamName = ParamName Named deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )
data FieldName = FieldName Named deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )
data ClassName = ClassName Named deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )
data SuperName = SuperName Named deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )
data MembrName = MembrName Named deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord, ToJSONKey, FromJSONKey )
data MethdName = MethdName Named deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord, ToJSONKey, FromJSONKey )
data NominalTy = NominalTy Named deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

-- getters signatures (to quiet ghc -Wall)
getVarNameToken   :: VarName   -> Named
getPkgNameToken   :: PkgName   -> Named
getFuncNameToken  :: FuncName  -> Named
getParamNameToken :: ParamName -> Named
getFieldNameToken :: FieldName -> Named
getClassNameToken :: ClassName -> Named
getSuperNameToken :: SuperName -> Named
getMembrNameToken :: MembrName -> Named
getMethdNameToken :: MethdName -> Named
getNominalTyToken :: NominalTy -> Named

-- getters
getVarNameToken   (VarName   v) = v
getPkgNameToken   (PkgName   p) = p
getFuncNameToken  (FuncName  f) = f
getParamNameToken (ParamName p) = p
getFieldNameToken (FieldName f) = f
getClassNameToken (ClassName c) = c
getSuperNameToken (SuperName s) = s
getMembrNameToken (MembrName m) = m
getMethdNameToken (MethdName m) = m
getNominalTyToken (NominalTy t) = t

-- locations signatures
getVarNameLocation :: VarName -> Location
getFuncNameLocation :: FuncName -> Location
getClassNameLocation :: ClassName -> Location
getParamNameLocation :: ParamName -> Location
getFieldNameLocation :: FieldName -> Location
getMethodNameLocation :: MethdName -> Location

-- locations signatures
getVarNameLocation = location . getVarNameToken
getFuncNameLocation = location . getFuncNameToken
getClassNameLocation = location . getClassNameToken
getParamNameLocation = location . getParamNameToken
getFieldNameLocation = location . getFieldNameToken
getMethodNameLocation = location . getMethdNameToken

data ConstBool
   = ConstBool
   {
       constBoolValue :: Bool,
       constBoolLocation :: Location
   }
   deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )

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
