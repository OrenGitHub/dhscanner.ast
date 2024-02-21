{-# LANGUAGE DeriveGeneric #-}

module Main

where

-- project imports
import Ast
import Token
import Location

-- general imports
import Data.List
import Test.QuickCheck

prop_varName :: VarName -> Bool
prop_varName v = let s = content (getVarNameToken v) in
    length s <= 2 || ((isPrefixOf "a" s) == False)

-- prop_varName v = content (getVarNameToken v) == "mishmish"

instance Arbitrary VarName
    where arbitrary = VarName <$> arbitrary

instance Arbitrary Named
    where arbitrary = Named <$> arbitrary <*> arbitrary

instance Arbitrary Location
    where arbitrary = Location <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary 

main :: IO ()
main = quickCheck prop_varName
