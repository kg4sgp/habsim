{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (mzero, when)
import Data.Char (isDigit)
import Data.Csv
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Time
import qualified Data.Vector as V
import System.Environment

data Direction = UGRD | VGRD deriving (Eq, Show)

newtype GribTime = GribTime UTCTime
  deriving (Eq, Ord, Read, Show, ParseTime, FormatTime)

data GribLine =
  GribLine { referenceTime :: GribTime
           , forecastTime  :: GribTime
           , direction     :: Direction
           , pressure      :: Int
           , latitude      :: Double
           , longitude     :: Double
           , velocity      :: Double
           } deriving (Eq, Show)

instance FromField Direction where
  parseField "UGRD" = pure UGRD
  parseField "VGRD" = pure VGRD
  parseField _      = mzero

instance FromField GribTime where
  parseField t =
    maybe mzero pure (parseTimeM True defaultTimeLocale "%F %T" (B.unpack t))

instance FromRecord GribLine where
  parseRecord v
    | V.length v == 7 =
      GribLine <$>
      v .! 0 <*>
      v .! 1 <*>
      v .! 2 <*>
      (mbToInt <$> v .! 3) <*>
      v .! 4 <*>
      v .! 5 <*>
      v .! 6
    | otherwise = mzero
    where
      mbToInt s = read (takeWhile isDigit s)

decodeGrib :: BL.ByteString -> Either String (V.Vector GribLine)
decodeGrib = decode NoHeader

filterGrib
  :: Double -- ^ Latitude
  -> Double -- ^ Longitude
  -> Int -- ^ Pressure
  -> V.Vector GribLine -- ^ Input lines
  -> V.Vector GribLine -- ^ Output lines
filterGrib lat lon pressure' gribLines =
  let lat' = fromIntegral (round (lat * 4) :: Integer) / 4
      lon' = fromIntegral (round (lon * 4) :: Integer) / 4
  in V.filter (\x -> latitude x == lat' &&
                   longitude x == lon' &&
                   pressure x == pressure') gribLines

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
