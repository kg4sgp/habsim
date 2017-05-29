{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module : Data.HABSim.Grib2.CSVParse.Types
-- Copyright : (C) 2017 Ricky Elrod
-- License : (see project LICENSE file)
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
-- Portability : GeneralizedNewtypeDeriving
--
-- This module provides types for parsing CSV files generated by @wgrib2@.
-- It is used (and re-exported) by 'Data.HABSim.Grib2.CSVParse'.
----------------------------------------------------------------------------
module Data.HABSim.Grib2.CSVParse.Types where

import Control.Applicative
import Control.Monad (mzero)
import qualified Data.ByteString.Char8 as B
import Data.Char (isDigit)
import Data.Csv
import Data.Time
import qualified Data.Vector as V

-- | The wind direction
--
-- 'UGRD' is North\/South, 'VGRD' is East\/West.
data Direction = UGRD | VGRD deriving (Eq, Show)

-- | This is used for both the 'referenceTime' and the 'forecastTime'. The
-- reason it exists is solely so we can create a 'FromField' instance on
-- 'UTCTime' while avoiding orphan instances.
newtype GribTime = GribTime UTCTime
  deriving (Eq, Ord, Read, Show, ParseTime, FormatTime)

-- | This is a (mostly-)raw gribline, right after being parsed.
--
-- The "mostly-" comes from the fact that we wrap a few things (e.g. the times
-- into 'GribTime') and convert 'pressure' into an 'Int'.
data RawGribLine =
  RawGribLine { referenceTime :: GribTime
              , forecastTime  :: GribTime
              , direction     :: Direction
              , pressure      :: Int
              , latitude      :: Double
              , longitude     :: Double
              , velocity      :: Double
              } deriving (Eq, Show)

-- | A single 'UGRD' line.
newtype UGRDLine = UGRDLine RawGribLine deriving (Eq, Show)

-- | A single 'VGRD' line.
newtype VGRDLine = VGRDLine RawGribLine deriving (Eq, Show)

-- | Either a 'UGRDLine' or a 'VGRDLine'. This is so we can parse and ultimately
-- return a 'V.Vector' containing both 'UGRD' and 'VGRD' lines. We return a
-- 'V.Vector' 'GribLine' (or rather, Cassava does) and we just know that a
-- 'GribLine' will either be a 'UGRDLine' or a 'VGRDLine'.
data GribLine = UGRDGribLine UGRDLine
              | VGRDGribLine VGRDLine
  deriving (Eq, Show)

-- | A pair of Grib lines, where one is a 'UGRDLine' and one is a 'VGRDLine'.
-- This is useful for filtering (see 'filterGrib' below) where we want to return
-- one of each and ensure through type safety that we don't get them confused.
data GribPair =
  GribPair { uGrd :: UGRDLine
           , vGrd :: VGRDLine
           } deriving (Eq, Show)

instance FromField Direction where
  parseField (B.unpack -> "UGRD") = pure UGRD
  parseField (B.unpack -> "VGRD") = pure VGRD
  parseField _      = mzero
  {-# INLINE parseField #-}

instance FromField GribTime where
  parseField t =
    maybe mzero pure (parseTimeM True defaultTimeLocale "%F %T" (B.unpack t))
  {-# INLINE parseField #-}

instance FromRecord GribLine where
  parseRecord v
    | V.length v == 7 =
      do
        refTime <- v .! 0
        foreTime <- v .! 1
        dir <- v .! 2
        press <- mbToInt <$> v .! 3
        lat <- v .! 4
        lon <- v .! 5
        vel <- v .! 6
        let rawLine = RawGribLine refTime foreTime dir press lat lon vel
        return $ case dir of
                   UGRD -> UGRDGribLine (UGRDLine rawLine)
                   VGRD -> VGRDGribLine (VGRDLine rawLine)
    | otherwise = mzero
    where
      mbToInt s = read (takeWhile isDigit s)

