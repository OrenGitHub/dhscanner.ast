{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Asts

where

import Data.Aeson
import GHC.Generics

-- project imports
import qualified Ast

data Asts
   = Asts
     {
         asts :: [ Ast.Root ]
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

