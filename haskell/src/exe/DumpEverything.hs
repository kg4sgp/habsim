module Main where

import Control.Monad.Writer
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Foldable (traverse_)
import Data.HABSim.HABSim hiding (pressure)
import Data.HABSim.Grib2.CSVParse
import System.Environment
import Utility

main :: IO ()
main = do
  csvName <- fmap head getArgs
  csv <- BL.readFile csvName
  let sv = SimVals 0.1 0.0
      pv = PosVel 41.1063 (-80.6477) (Altitude 300) 0.0 0.0 3.0
      bv = Bvars 2.0 0.47 1.0 0.5 0.0 540.0 (Liter 5.0) 120000.0
      w = Wind 4.0 4.0
      s = Simulation sv pv bv w
      gribLines = either error id (decodeGrib csv)
      pressures = nub (fmap (pressure . gribLineToRaw) gribLines)
      (lastAscent, accAscent) =
        runWriter $ sim Ascent s pressures gribLines
      (lastDescent, accDescent) =
        runWriter $ sim Descent (Simulation (retSV lastAscent) (retPV lastAscent) (retBV lastAscent) (retW lastAscent)) pressures gribLines
  traverse_ print accAscent
  print lastAscent
  traverse_ print accDescent
  print lastDescent
