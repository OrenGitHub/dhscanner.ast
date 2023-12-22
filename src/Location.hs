{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Location

where

import Data.Aeson
import GHC.Generics

data Location =
     Location
     {
         filename  :: FilePath,
         lineStart :: Int,
         lineEnd   :: Int,
         colStart  :: Int,
         colEnd    :: Int
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON )

