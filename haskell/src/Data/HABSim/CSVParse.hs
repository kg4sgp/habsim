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

data Direction = UGRD | VGRD deriving (Eq, Show)

newtype GribTime = GribTime UTCTime
  deriving (Eq, Ord, Read, Show, ParseTime, FormatTime)

data RawGribLine =
  RawGribLine { referenceTime :: GribTime
              , forecastTime  :: GribTime
              , direction     :: Direction
              , pressure      :: Int
              , latitude      :: Double
              , longitude     :: Double
              , velocity      :: Double
              } deriving (Eq, Show)

newtype UGRDLine = UGRDLine RawGribLine deriving (Eq, Show)
newtype VGRDLine = VGRDLine RawGribLine deriving (Eq, Show)

data GribLine = UGRDGribLine UGRDLine
              | VGRDGribLine VGRDLine
  deriving (Eq, Show)

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

decodeGrib :: BL.ByteString -> Either String (V.Vector GribLine)
decodeGrib = decode NoHeader
{-# INLINE decodeGrib #-}

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
