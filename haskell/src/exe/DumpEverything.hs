module Main where

import Control.Lens
import Control.Monad.Writer
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Foldable (traverse_)
import Data.HABSim.HABSim
import Data.HABSim.Grib2.CSVParse
import Data.HABSim.Lens
import Data.List (intercalate)
import qualified Data.HashMap.Lazy as HM
import Data.Time
import qualified Data.Vector as V
import System.Environment
import Utility

prettySV :: UTCTime -> SimulationTime -> String
prettySV startTime (SimulationTime _ t) =
  show $ addUTCTime (fromIntegral (round t :: Integer)) startTime

pretty :: UTCTime -> Simulation -> String
pretty startTime (Simulation sv pv b w) =
  intercalate "," $ [ prettySV startTime sv
                    , sv ^. increment . to show
                    , sv ^. simulationTime . to show
                    , pv ^. lat . to show . to (take 10)
                    , pv ^. lon . to show . to (take 10)
                    , pv ^. alt . altitude . to show . to (take 10)
                    , pv ^. vel_x . velocity . to show . to (take 10)
                    , pv ^. vel_y . velocity . to show . to (take 10)
                    , pv ^. vel_z . velocity . to show . to (take 10)
                    , b ^. mass . mass .to show . to (take 10)
                    , b ^. bal_cd . coeffDrag . to show . to (take 10)
                    , b ^. par_cd . coeffDrag . to show . to (take 10)
                    , b ^. packages_cd . coeffDrag . to show . to (take 10)
                    , b ^. launch_time . to show . to (take 10)
                    , b ^. burst_vol . liter . to show . to (take 10)
                    , b ^. b_vol . liter . to show . to (take 10)
                    , b ^. b_pres . pressure . to show . to (take 10)
                    , w ^. velo_x . windX . windMs . to show . to (take 10)
                    , w ^. velo_y . windY . windMs . to show . to (take 10)
                    ]

main :: IO ()
main = do
  csvName <- fmap head getArgs
  csv <- BL.readFile csvName
  let sv = SimulationTime 0.1 0.0
      pv = PosVel 41.1063 (-80.6477) (Altitude 300) 0.0 0.0 3.0
      bv = Burst 2.0 0.47 1.0 0.5 0.0 540.0 (Liter 5.0) 120000.0
      w = Wind 4.0 4.0
      s = Simulation sv pv bv w
      startTime = UTCTime (fromGregorian 2017 05 28) 43200
      gribLines = either error keyedGribToHM (decodeKeyedGrib csv)
      pressures =
        nub
        (fmap (\x -> gribLineToRaw x ^. pressure)
        (V.fromList . HM.elems $ gribLines))
      (lastAscent, accAscent) =
        runWriter $ sim Ascent s pressures gribLines (const True)
      ascentLastSim =
        Simulation
        (lastAscent ^. retSV)
        (lastAscent ^. retPV)
        (lastAscent ^. retBV)
        (lastAscent ^. retW)
      (lastDescent, accDescent) =
        runWriter $ sim Descent ascentLastSim pressures gribLines (const True)
  traverse_ (putStrLn . pretty startTime) accAscent
  putStrLn . pretty startTime $ lastAscent
  traverse_ (putStrLn . pretty startTime) accDescent
  putStrLn . pretty startTime $ lastDescent
