module Data.HABSim.HABSim
  ( module Data.HABSim.Types
  , sim
  ) where

import Control.Lens
import Control.Monad.Writer
import qualified Data.DList as D
import qualified Data.HABSim.Internal as I
import Data.HABSim.Lens
import Data.HABSim.Types
import Data.HABSim.Grib2.CSVParse.Types
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V

sim
  :: Pitch
  -> Simulation
  -> V.Vector Int -- ^ Vector of pressures to round to from Grib file
  -> HM.HashMap Key GribLine
  -> (Simulation -> Bool) -- ^ We record the line when this predicate is met
  -> Writer (D.DList Simulation) Simulation
sim p
    simul@(Simulation
            sv
            (PosVel lat' lon' alt' vel_x' vel_y' vel_z')
            (Burst mass' bal_cd' par_cd' packages_cd' launch_time' burst_vol' b_volume' b_press')
            (Wind (WindX wind_x') (WindY wind_y')))
    pressureList
    gribLines
    tellPred
  | baseGuard p = do
    let pv = PosVel lat' lon' alt' vel_x' vel_y' vel_z'
        bv = Burst mass' bal_cd' par_cd' packages_cd' launch_time' burst_vol' b_volume' b_press'
        w = Wind windX' windY'
    return (Simulation sv pv bv w)
  | otherwise = do
    let sv' = sv { _simulationTime = sv ^. simulationTime + sv ^. increment }
        pv = PosVel nlat nlon nAlt nvel_x nvel_y (pitch p vel_z' nvel_z)
        bv = Burst mass' bal_cd' par_cd' packages_cd' launch_time' burst_vol' (pitch p nVol b_volume') (pitch p pres b_press')
        w = Wind windX' windY'
        s = Simulation sv' pv bv w
    when (tellPred simul) $
      tell (D.singleton s)
    sim p s pressureList gribLines tellPred
  where
    -- The guard to use depends on the pitch
    baseGuard Ascent = b_volume' >= burst_vol'
    baseGuard Descent = alt' < 0

    -- Getting pressure and density at current altitude
    PressureDensity pres dens = I.altToPressure alt'

    -- Calculating volume, radius, and crossectional area
    nVol = I.newVolume b_press' b_volume' pres
    Meter nbRad = I.spRadFromVol nVol
    nCAsph  = I.cAreaSp nbRad

    -- Calculate drag force for winds
    f_drag_x =
      case p of
        Ascent -> I.drag dens vel_x' (_windX windX') bal_cd' nCAsph
        Descent -> I.drag dens vel_x' (_windX windX') packages_cd' 1
    f_drag_y =
      case p of
        Ascent -> I.drag dens vel_y' (_windY windY') bal_cd' nCAsph
        Descent -> I.drag dens vel_y' (_windY windY') packages_cd' 1
    -- Only used for descent
    f_drag_z = I.drag dens vel_z' 0 par_cd' 1

    -- Net forces in z
    f_net_z = f_drag_z - (I.force mass' I.g)


    -- Calculate Kenimatics
    accel_x = I.accel f_drag_x mass'
    accel_y = I.accel f_drag_y mass'
    accel_z = I.accel f_net_z mass'
    nvel_x = I.velo vel_x' accel_x sv
    nvel_y = I.velo vel_y' accel_y sv
    nvel_z = I.velo vel_z' accel_z sv
    Altitude disp_x = I.displacement (Altitude 0.0) nvel_x accel_x sv
    Altitude disp_y = I.displacement (Altitude 0.0) nvel_y accel_y sv
    nAlt = I.displacement alt' vel_z' 0.0 sv

    -- Calculate change in corrdinates
    -- Because of the relatively small changes, we assume a spherical earth
    bearing = atan2 disp_y disp_x
    t_disp = (disp_x ** 2 + disp_y ** 2) ** (1 / 2)
    ang_dist = t_disp / I.er
    
    latr = lat' * (pi / 180)
    lonr = lon' * (pi / 180)
    nlatr =
      asin (sin latr * cos ang_dist + cos latr * sin ang_dist * cos bearing)
    nlonr =
      lonr +
      atan2 (sin bearing * sin ang_dist * cos latr)
            (cos ang_dist - (sin latr * sin nlatr))
    nlat = nlatr * (180 / pi)
    nlon = nlonr * (180 / pi)

    ((la1, lo1), (la2, lo2), (la3, lo3), (la4, lo4)) =
      I.latLonBox (Latitude lat') (Longitude lon') 0.25

    filterPressure = I.roundToClosest pres pressureList

    (windX', windY') =
      fromMaybe
      (WindX wind_x', WindY wind_y')
      (I.windFromLatLon
        (Latitude lat')
        (Longitude lon')
        filterPressure
        gribLines)
