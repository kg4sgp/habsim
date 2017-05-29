module Main where

import Control.Monad.Writer
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.DList as D
import Data.HABSim.HABSim hiding (pressure)
import Data.HABSim.Grib2.CSVParse
import Data.List (intercalate)
import System.Environment

-- {lat: -18.142, lng: 178.431}
jsonLatLon :: Breturn -> String
jsonLatLon (Breturn _ (PosVel lat' lon' _ _ _ _) _ _) =
  "{lat: " ++ show lat' ++ ", lng: " ++ show lon' ++ "}"

main :: IO ()
main = do
  csvName <- fmap head getArgs
  csv <- BL.readFile csvName
  let sv = SimVals 0.1 0.0
      pv = PosVel 41.1063 (-80.6477) (Altitude 300) 0.0 0.0 3.0
      bv = Bvars 2.0 0.47 1.0 0.5 0.0 540.0 (Liter 5.0) 120000.0
      w = Wind 4.0 4.0
      gribLines = either error id (decodeGrib csv)
      pressures = fmap (pressure . gribLineToRaw) gribLines
      (lastAscent@(Breturn sv' pv' bv' w'), accAscent) =
        runWriter $ sim Ascent sv pv bv w pressures gribLines
      (lastDescent, accDescent) =
        runWriter $ sim Descent sv' pv' bv' w' pressures gribLines
      ascent = D.cons lastAscent accAscent
      descent = D.cons lastDescent accDescent
  putStrLn "var flight_path = ["
  putStrLn . intercalate ",\n" . map jsonLatLon . D.toList $
    ascent `D.append` descent
  putStrLn "];"
