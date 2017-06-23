module Main where

import Control.Lens
import Control.Monad.Writer
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.DList as D
import Data.HABSim.HABSim
import Data.HABSim.Grib2.CSVParse
import Data.HABSim.Lens
import Data.HABSim.VectorUtilities
import qualified Data.HashMap.Lazy as HM
import Data.List (intercalate)
import qualified Data.Vector as V
import System.Environment

-- {lat: -18.142, lng: 178.431}
jsonLatLon :: Simulation -> String
jsonLatLon (Simulation _ (PosVel lat' lon' _ _ _ _) _ _) =
  "{lat: " ++ show lat' ++ ", lng: " ++ show lon' ++ "}"

main :: IO ()
main = do
  csvName <- fmap head getArgs
  csv <- BL.readFile csvName
  let sv = SimulationTime 0.1 0.0
      pv = PosVel (40.3772)         -- lat (decimal degrees)
                  (-83.0594)        -- lon (decimal degrees)
                  (Altitude 300)    -- altitude (m)
                  0.0               -- velocity x initial (m/s)
                  0.0               -- velocity y initial (m/s)
                  4.0               -- velocity z (ascent rate) (m/s)
      bv = Burst  2.0               -- mass (kg)
                  0.47              -- Balloon Coefficent of Drag
                  1.0               -- Parachute Coefficent of Drag
                  0.5               -- Packages Coefficent of Drag
                  0.0               -- Launch Time (Not used currently)
                  550.0             -- Burst Volume (Liters)
                  (Liter 5.0)       -- Balloon Volume initial (Liters)
                  101325.0          -- Balloon Pressure initial (Pascals)
                  2.01588E-3        -- Lifting gas molar mass (hydrogen)
                  288.15            -- Lifting gas temperature
      w = Wind 4.0 4.0
      s = Simulation sv pv bv w
      tellPred simul =
        round (_simulationTime (_retSV simul)) `mod` 100 == (0 :: Integer)
      gribLines = either error keyedGribToHM (decodeKeyedGrib csv)
      pressures =
        nub
        (fmap (\x -> gribLineToRaw x ^. pressure)
        (V.fromList . HM.elems $ gribLines))
      (lastAscent, accAscent) =
        runWriter $ sim Ascent s pressures gribLines tellPred
      ascentLastSim =
        Simulation
        (lastAscent ^. retSV)
        (lastAscent ^. retPV)
        (lastAscent ^. retBV)
        (lastAscent ^. retW)
      (lastDescent, accDescent) =
        runWriter $ sim Descent ascentLastSim pressures gribLines tellPred
  putStrLn $ "var burst_point = " ++ jsonLatLon lastAscent ++ ";"
  putStrLn "var flight_path = ["
  putStr . intercalate ",\n" . map jsonLatLon . D.toList $ accAscent
  putStrLn ","
  putStr . jsonLatLon $ lastAscent
  putStrLn ","
  putStr . intercalate ",\n" . map jsonLatLon . D.toList $ accDescent
  putStrLn ","
  putStrLn . jsonLatLon $ lastDescent
  putStrLn "];"
