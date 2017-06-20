{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module : Data.HABSim.Grib2.CSVParse.Types
-- Copyright : (C) 2017 Ricky Elrod
-- License : (see project LICENSE file)
-- Maintainer : Ricky Elrod <ricky@elrod.me>
-- Stability : experimental
-- Portability : GeneralizedNewtypeDeriving, ViewPatterns
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
import Data.HABSim.Types (Longitude (..), Latitude (..))
import Data.Hashable
import Data.List (isSuffixOf)
import Data.Time
import qualified Data.Vector as V
import Text.Read (readMaybe)

-- | The wind direction
--
-- 'UGRD' is North\/South, 'VGRD' is East\/West.
--
-- If we're given something other than @UGRD@ or @VGRD@ in this field, we store
-- the value in the 'Other' constructor.
data Direction = UGRD | VGRD | Other String deriving (Eq, Show)

instance Hashable Direction where
  hashWithSalt s (Other str) = s `hashWithSalt` (0::Int) `hashWithSalt` str
  hashWithSalt s UGRD = s `hashWithSalt` (1::Int)
  hashWithSalt s VGRD = s `hashWithSalt` (2::Int)

-- | This is used for both the 'referenceTime' and the 'forecastTime'. The
-- reason it exists is solely so we can create a 'FromField' instance on
-- 'UTCTime' while avoiding orphan instances.
newtype GribTime = GribTime { _gribTime :: UTCTime }
  deriving (Eq, Ord, Read, Show, ParseTime, FormatTime)

-- | This is a (mostly-)raw gribline, right after being parsed.
--
-- The "mostly-" comes from the fact that we wrap a few things (e.g. the times
-- into 'GribTime') and convert 'pressure' into an 'Int'.
data RawGribLine =
  RawGribLine { _referenceTime :: GribTime
              , _forecastTime  :: GribTime
              , _direction     :: Direction
              , _pressure      :: Int
              , _longitude     :: Longitude
              , _latitude      :: Latitude
              , _velocity      :: Double
              } deriving (Eq, Show)

-- | A single 'UGRD' line.
newtype UGRDLine = UGRDLine { _uGRDLineRaw :: RawGribLine } deriving (Eq, Show)

-- | A single 'VGRD' line.
newtype VGRDLine = VGRDLine { _vGRDLineRaw :: RawGribLine } deriving (Eq, Show)

-- | A single non-UGRD and non-VGRD line.
newtype OtherLine =
  OtherLine { _otherLineRaw :: RawGribLine } deriving (Eq, Show)

-- | Either a 'UGRDLine' or a 'VGRDLine'. This is so we can parse and ultimately
-- return a 'V.Vector' containing both 'UGRD' and 'VGRD' lines. We return a
-- 'V.Vector' 'GribLine' (or rather, Cassava does) and we just know that a
-- 'GribLine' will either be a 'UGRDLine' or a 'VGRDLine'.
--
-- If the line is anything else, we return the raw line in 'OtherGribLine'.
data GribLine = UGRDGribLine UGRDLine
              | VGRDGribLine VGRDLine
              | OtherGribLine OtherLine
  deriving (Eq, Show)

instance FromField Direction where
  parseField (B.unpack -> "UGRD") = pure UGRD
  parseField (B.unpack -> "VGRD") = pure VGRD
  parseField s = pure (Other (B.unpack s))
  {-# INLINE parseField #-}

instance FromField GribTime where
  parseField t =
    maybe mzero pure (parseTimeM True defaultTimeLocale "%F %T" (B.unpack t))
  {-# INLINE parseField #-}

type Pressure = Int
type Key = (Longitude, Latitude, Pressure, Direction)

newtype KeyedGribLine =
  KeyedGribLine (Key, GribLine)
  deriving (Eq, Show)

instance FromRecord KeyedGribLine where
  parseRecord v
    | V.length v == 7 =
      do
        refTime <- v .! 0
        foreTime <- v .! 1
        dir <- v .! 2
        press <- parsePressure <$> v .! 3
        lon <- v .! 4
        lat <- v .! 5
        vel <- v .! 6
        case press of
          Nothing -> mzero
          Just press' ->
            let rawLine = RawGribLine refTime foreTime dir press' lon lat vel
                key = (lon, lat, press', dir)
            in return $
               case dir of
                 UGRD ->
                   KeyedGribLine (key, (UGRDGribLine (UGRDLine rawLine)))
                 VGRD ->
                   KeyedGribLine (key, (VGRDGribLine (VGRDLine rawLine)))
                 Other _ ->
                   KeyedGribLine (key, (OtherGribLine (OtherLine rawLine)))
    | otherwise = mzero
    where
      parsePressure s =
        if " mb" `isSuffixOf` s
        then readMaybe (takeWhile isDigit s)
        else Nothing
