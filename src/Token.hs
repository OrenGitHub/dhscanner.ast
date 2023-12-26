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
       locationNamed :: Location
   }
   deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )
   
data VarName   = VarName   Named deriving ( Show, Eq, Generic, ToJSON, FromJSON )
data FuncName  = FuncName  Named deriving ( Show, Eq, Generic, ToJSON, FromJSON )
data ParamName = ParamName Named deriving ( Show, Eq, Generic, ToJSON, FromJSON )
data FieldName = FieldName Named deriving ( Show, Eq, Generic, ToJSON, FromJSON )
data ClassName = ClassName Named deriving ( Show, Eq, Generic, ToJSON, FromJSON )
data SuperName = SuperName Named deriving ( Show, Eq, Generic, ToJSON, FromJSON )
data MembrName = MembrName Named deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord, ToJSONKey, FromJSONKey )
data MethdName = MethdName Named deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord, ToJSONKey, FromJSONKey )
data NominalTy = NominalTy Named deriving ( Show, Eq, Generic, ToJSON, FromJSON )

-- signatures (to quiet ghc -Wall)
getVarNameToken   :: VarName   -> Named
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
getFuncNameToken  (FuncName  f) = f
getParamNameToken (ParamName p) = p
getFieldNameToken (FieldName f) = f
getClassNameToken (ClassName c) = c
getSuperNameToken (SuperName s) = s
getMembrNameToken (MembrName m) = m
getMethdNameToken (MethdName m) = m
getNominalTyToken (NominalTy t) = t

data ConstInt
   = ConstInt
   {
       constIntValue :: Int,
       constIntLocation :: Location
   }
   deriving ( Show, Eq, Generic, ToJSON, FromJSON )

data ConstStr
   = ConstStr
   {
       constStrValue :: String,
       constStrLocation :: Location
   }
   deriving ( Show, Eq, Generic, ToJSON, FromJSON )
