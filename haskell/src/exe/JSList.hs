module Main where

import Control.Monad.Writer
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.DList as D
import Data.HABSim.HABSim hiding (pressure)
import Data.HABSim.Grib2.CSVParse
import Data.List (intercalate)
import System.Environment
import Utility

-- {lat: -18.142, lng: 178.431}
jsonLatLon :: Simulation -> String
jsonLatLon (Simulation _ (PosVel lat' lon' _ _ _ _) _ _) =
  "{lat: " ++ show lat' ++ ", lng: " ++ show lon' ++ "}"

main :: IO ()
main = do
  csvName <- fmap head getArgs
  csv <- BL.readFile csvName
  let sv = SimulationTime 0.1 0.0
      pv = PosVel 41.1063 (-80.6477) (Altitude 300) 0.0 0.0 3.0
      bv = Burst 2.0 0.47 1.0 0.5 0.0 540.0 (Liter 5.0) 120000.0
      w = Wind 4.0 4.0
      s = Simulation sv pv bv w
      tellPred simul =
        round (simulationTime (retSV simul)) `mod` 100 == (0 :: Integer)
      gribLines = either error id (decodeGrib csv)
      pressures = nub (fmap (pressure . gribLineToRaw) gribLines)
      (lastAscent, accAscent) =
        runWriter $ sim Ascent s pressures gribLines tellPred
      (lastDescent, accDescent) =
        runWriter $ sim Descent (Simulation (retSV lastAscent) (retPV lastAscent) (retBV lastAscent) (retW lastAscent)) pressures gribLines tellPred
  putStrLn "var flight_path = ["
  putStr . intercalate ",\n" . map jsonLatLon . D.toList $ accAscent
  putStrLn ","
  putStr . jsonLatLon $ lastAscent
  putStrLn ","
  putStr . intercalate ",\n" . map jsonLatLon . D.toList $ accDescent
  putStrLn ","
  putStrLn . jsonLatLon $ lastDescent
  putStrLn "];"
