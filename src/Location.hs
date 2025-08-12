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

-- |
-- Lines and columns are 1-based, for compatability with
-- [Sarif](https://docs.oasis-open.org/sarif/sarif/v2.1.0/errata01/os/sarif-v2.1.0-errata01-os-complete.html#_Toc141790937).
--
-- > The line number of the first line in a text artifact SHALL be 1.
data Location =
     Location
     {
         filename  :: FilePath,
         lineStart :: Word,
         lineEnd   :: Word,
         colStart  :: Word,
         colEnd    :: Word
     }
     deriving ( Show, Read, Eq, Generic, ToJSON, FromJSON, Ord )
