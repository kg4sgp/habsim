{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.HABSim.CSVParse where

import Control.Monad (mzero)
import Data.Char (isDigit)
import Data.Csv
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
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
  parseField "UGRD" = pure UGRD
  parseField "VGRD" = pure VGRD
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

-- | A helper function that primarily exists just to help type inference.
--
-- @decodeGrib = decode NoHeader@ (where 'decode' and 'NoHeader' both come from
-- Cassava).
decodeGrib :: BL.ByteString -> Either String (V.Vector GribLine)
decodeGrib = decode NoHeader
{-# INLINE decodeGrib #-}

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
  where
    gribLineToRaw (UGRDGribLine (UGRDLine l)) = l
    gribLineToRaw (VGRDGribLine (VGRDLine l)) = l


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
