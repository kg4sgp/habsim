-----------------------------------------------------------------------------
-- |
-- Module : Data.HABSim.Grib2.CSVParse.Types
-- Copyright : (C) 2017 Ricky Elrod
-- License : (see project LICENSE file)
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
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
  , gribLineToRaw
  -- * Keyed/HashMap-based Grib data
  , decodeKeyedGrib
  , keyedGribToHM
  , filterKeyedGrib
  ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Csv
import Data.HABSim.Grib2.CSVParse.Types
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V

-- | A helper function that primarily exists just to help type inference.
--
-- @decodeKeyedGrib = decode NoHeader@ (where 'decode' and 'NoHeader' both come
-- from Cassava).
decodeKeyedGrib :: BL.ByteString -> Either String (V.Vector KeyedGribLine)
decodeKeyedGrib = decode NoHeader
{-# INLINE decodeKeyedGrib #-}

-- | Convert a 'V.Vector' of 'KeyedGribLine' into a 'HM.HashMap' keyed on the
-- latitude longitude, pressure, and direction of the grib line.
keyedGribToHM :: V.Vector KeyedGribLine -> HM.HashMap Key GribLine
keyedGribToHM = V.foldr (\(KeyedGribLine (key, gline)) hm ->
                            HM.insert key gline hm) HM.empty

-- | Given any kind of 'GridLine' (usually either a 'UGRDGribLine' or a
-- 'VGRDGribLine', but could also be an 'OtherGribLine'), pull the raw Grib line
-- out of it.
gribLineToRaw :: GribLine -> RawGribLine
gribLineToRaw (UGRDGribLine (UGRDLine l)) = l
gribLineToRaw (VGRDGribLine (VGRDLine l)) = l
gribLineToRaw (OtherGribLine l) = l

-- | Filter Grib lines from a 'V.Vector' 'GribLine'.
-- If we for some reason don't have both 'UGRD' and 'VGRD' of our filter result,
-- then we return 'Nothing'. Otherwise we return a 'GribPair' containing both.
filterKeyedGrib
  :: Double -- ^ Latitude
  -> Double -- ^ Longitude
  -> Int -- ^ Pressure
  -> Direction -- ^ The 'Direction' to filter for.
  -> HM.HashMap Key GribLine -- ^ Input lines
  -> Maybe GribLine -- ^ Output line
filterKeyedGrib lat lon pressure' dir gribLines = do
  let lat' = fromIntegral (round (lat * 4) :: Integer) / 4
      lon' = fromIntegral (round (lon * 4) :: Integer) / 4
  HM.lookup (lat', lon', pressure', dir) gribLines
