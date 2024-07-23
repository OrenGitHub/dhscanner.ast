-- |
--
-- * Locations exist for all "code entities"
--
--     * variables
--     * function calls
--     * function definitions
--     * while loops
--     * etc.
-- 
-- * Locations are constructed /only during parsing/
--
--     * users should /not/ construct locations
--
-- * Locations help to present security findings to users
--
-- * In addition, locations are used as /primary keys/ in the container database
--
--     * more precisely, locations are /variables/ in the resulting __Prolog__ program
--     * each location corresponds to /exactly one/ "code entity"
--     * this is why users should /never/ construct locations
--     * it is completely the parser's job !
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Location

where

import Data.Aeson
import GHC.Generics

-- | Lines and columns are 1-based, like in most IDEs.
data Location =
     Location
     {
         filename  :: FilePath,
         lineStart :: Word,
         lineEnd   :: Word,
         colStart  :: Word,
         colEnd    :: Word
     }
     deriving ( Show, Eq, Generic, ToJSON, FromJSON, Ord )
