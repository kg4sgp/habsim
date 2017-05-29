-----------------------------------------------------------------------------
-- |
-- Module : Data.HABSim.Grib2.CSVParse.Types
-- Copyright : (C) 2017 Ricky Elrod
-- License : (see project LICENSE file)
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
-- Portability : GeneralizedNewtypeDeriving
--
-- This module provides utilities for parsing and filtering data from a
-- @wgrib2@-generated CSV file. Once the CSV files is generated, it can be
-- parsed and filtered in the following way (preferably with better error
-- handling):
--
-- @
--   myCsv <- 'BL.readFile' "\/path\/to\/csv.csv"
--   case 'decodeGrib' myCsv of
--     Left str -> error str
--     Right gribLines ->
--       case 'filterGrib' 38.8977 (-77.0365) 950 gribLines of
--         Nothing -> error "No entry found"
--         Just wh -> print wh
-- @
----------------------------------------------------------------------------
module Data.HABSim.Grib2.CSVParse
  ( module Data.HABSim.Grib2.CSVParse.Types
  , decodeGrib
  , gribLineToRaw
  , filterGrib
  ) where

import Control.Monad (mzero)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Csv
import Data.HABSim.Grib2.CSVParse.Types
import qualified Data.Vector as V

-- | A helper function that primarily exists just to help type inference.
--
-- @decodeGrib = decode NoHeader@ (where 'decode' and 'NoHeader' both come from
-- Cassava).
decodeGrib :: BL.ByteString -> Either String (V.Vector GribLine)
decodeGrib = decode NoHeader
{-# INLINE decodeGrib #-}

-- | Given any kind of 'GridLine' (either a 'UGRDGribLine' or a 'VGRDGribLine'),
-- pull the raw Grib line out of it.
gribLineToRaw :: GribLine -> RawGribLine
gribLineToRaw (UGRDGribLine (UGRDLine l)) = l
gribLineToRaw (VGRDGribLine (VGRDLine l)) = l
gribLineToRaw (OtherGribLine l) = l

-- | Filter Grib lines from a 'V.Vector' 'GribLine'.
-- If we for some reason don't have both 'UGRD' and 'VGRD' of our filter result,
-- then we return 'Nothing'. Otherwise we return a 'GribPair' containing both.
filterGrib
  :: Double -- ^ Latitude
  -> Double -- ^ Longitude
  -> Int -- ^ Pressure
  -> V.Vector GribLine -- ^ Input lines
  -> Maybe GribPair -- ^ Output lines (both UGRD and VGRD)
filterGrib lat lon pressure' gribLines = do
  let lat' = fromIntegral (round (lat * 4) :: Integer) / 4
      lon' = fromIntegral (round (lon * 4) :: Integer) / 4
      filteredLines =
        V.filter (\x -> let raw = gribLineToRaw x
                        in latitude raw == lat' &&
                           longitude raw == lon' &&
                           pressure raw == pressure') gribLines
  first <- filteredLines V.!? 0
  second <- filteredLines V.!? 1
  case first of
    UGRDGribLine u ->
      case second of
        VGRDGribLine v -> return (GribPair u v)
        _ -> mzero
    VGRDGribLine v ->
      case second of
        UGRDGribLine u -> return (GribPair u v)
        _ -> mzero
    _ -> mzero

{-
main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) (error "Must pass CSV file to use")
  f <- BL.readFile (head args)
  case decodeGrib f of
    Left e -> error e
    Right v ->
      let filteredLines = filterGrib (-80) (40.32) 925 v
      in print filteredLines
-}
