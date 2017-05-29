{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.HABSim.HABSim where

import Control.Monad (when)
import Control.Monad.Writer
import qualified Data.DList as D
import Data.HABSim.Grib2.CSVParse
import qualified Data.Vector as V

#define DoubleGND Enum, Eq, Floating, Fractional, Num, Ord, Read, Real, \
  RealFloat, RealFrac, Show

-- I'd like to see us get rid of these and maybe use something like the
-- @dimensional@ package instead. We can still wrap them in newtypes, but we'll
-- get extra type-safety this way, at the cost of possibly some syntactical
-- annoyance. The trade-off seems worth it to me.
newtype Meter = Meter Double deriving (DoubleGND)
newtype Altitude = Altitude Double deriving (DoubleGND)
newtype Latitude  = Latitude Double deriving (DoubleGND)
newtype Longitude = Longitude Double deriving (DoubleGND)
newtype Pressure = Pressure Double deriving (DoubleGND)
newtype Density = Density Double deriving (DoubleGND)
newtype Liter = Liter Double deriving (DoubleGND)
newtype WindMs = WindMs Double deriving (DoubleGND)
newtype CoeffDrag = CoeffDrag Double deriving (DoubleGND)
newtype Velocity = Velocity Double deriving (DoubleGND)
newtype CrossSecArea = CrossSecArea Double deriving (DoubleGND)
newtype Force = Force Double deriving (DoubleGND)
newtype Acceleration = Acceleration {acceleration :: Double} deriving (DoubleGND)
newtype Mass = Mass Double deriving (DoubleGND)
newtype Displacement = Displacement Double deriving (DoubleGND)

-- Useful aliases
type Volume = Liter
type SphericalRadius = Meter

data Coordinate = Coordinate Latitude Longitude Altitude deriving (Show, Eq, Ord)

data AzElCord =
  AzelCord { azimuth    :: !Double
           , elevation  :: !Double
           , range      :: !Double
           }

data PressureDensity =
  PressureDensity { pressure :: Pressure
                  , density :: Density
                  } deriving (Eq, Ord, Show)

data AltitudeRegionValues =
  AltitudeRegionValues { hb  :: !Double
                       , tb  :: !Double
                       , lb  :: !Double
                       , pb  :: !Double
                       , rho :: !Double
                       } deriving (Eq, Ord, Show)

data SimVals =
  SimVals { t_inc     :: !Double
          , t         :: !Double
          } deriving (Eq, Ord, Show)

data PosVel =
  PosVel {  lat       :: !Double
          , lon       :: !Double
          , alt       :: Altitude
          , vel_x     :: Velocity
          , vel_y     :: Velocity
          , vel_z     :: Velocity
          } deriving (Eq, Ord, Show)

data Bvars =
  Bvars { mass          :: Mass
        , bal_cd        :: CoeffDrag
        , par_cd        :: CoeffDrag
        , packages_cd   :: CoeffDrag
        , launch_time   :: !Double
        , burst_vol     :: Volume
        , b_vol         :: Volume
        , b_pres        :: Pressure
        } deriving (Eq, Ord, Show)

data Wind =
  Wind { velo_x         :: WindMs
       , velo_y         :: WindMs
       } deriving (Eq, Ord, Show)

data Breturn =
  Breturn { retSV :: SimVals
          , retPV :: PosVel
          , retBV :: Bvars
          , retW  :: Wind
          } deriving (Eq, Ord, Show)

data Pitch = Ascent | Descent

foldPitch :: Pitch -> a -> a -> a
foldPitch Ascent a _ = a
foldPitch Descent _ b = b

-- | constants
m, r, er :: Double
m   = 0.0289644
r   = 8.3144598
er  = 6378137.0

g :: Acceleration
g   = 9.80665

-- | Calculate new volume given an initial pressure and volume, and a new
-- pressure
newVolume :: Pressure -> Volume -> Pressure -> Volume
newVolume (Pressure p1) (Liter v) (Pressure p2) = Liter $ (p1 * v) / p2

-- | Calculate sphereical radius from volume
spRadFromVol :: Volume -> SphericalRadius
spRadFromVol (Liter v) = Meter $ ((3 * v) / (4 * pi)) ** (1 / 3)

-- Calculate cross sectional area of sphere.
cAreaSp :: Double -> CrossSecArea
cAreaSp r = CrossSecArea $ pi * (r ** 2)

-- Calculate gas density given molar mass, temp and pressure.
gas_dens :: Double -> Double -> Double -> Double
gas_dens mm p t = (mm*p)/(r*t)

-- | Calculate boyancy.
boyancy :: Double -> Double -> Double -> Double
boyancy p_air p_gas v = (p_air-p_gas)*(acceleration g)*v

-- Calculate drag.
drag :: Density -> Velocity -> WindMs -> CoeffDrag -> CrossSecArea -> Force
drag (Density d) (Velocity v) (WindMs w) (CoeffDrag c) (CrossSecArea a) =
  Force $ (1 / 2) * d * ((v - w) ** 2) * c * a

-- Calculate acceleration.
accel :: Force -> Mass -> Acceleration
accel (Force f) (Mass m) = Acceleration $ f / m

force :: Mass -> Acceleration -> Force
force (Mass m) (Acceleration a) = Force $ m * a

-- Calculate velocity.
velo :: Velocity -> Acceleration -> SimVals -> Velocity
velo (Velocity v) (Acceleration a) (SimVals t _) = Velocity $ v + a * t

-- Calculate displacement.
displacement :: Altitude -> Velocity -> Acceleration -> SimVals -> Altitude {-Displacement-}
displacement (Altitude x) (Velocity v) (Acceleration a) (SimVals t _) =
  Altitude $ x + (v * t) + ((1 / 2) * a * t ** 2)

altToValues :: Altitude -> AltitudeRegionValues
altToValues (Altitude alt)
  --                                    alt(m)Temp(K)  Lapse Rate    Pres(Pa)  Dens(kg/m^3)
  | alt <= (-500) = error "Altitude out of range, too low (<500m)"
  | alt <  11000  = AltitudeRegionValues 0     288.15  (-0.0065)     101325    1.225
  | alt <= 20000  = AltitudeRegionValues 11000 216.65  0             22632.1   0.36391
  | alt <= 32000  = AltitudeRegionValues 20000 216.65  0.001         5474.89   0.08803
  | alt <= 47000  = AltitudeRegionValues 32000 228.65  0.0028        868.02    0.01322
  | alt <= 51000  = AltitudeRegionValues 47000 270.65  0             110.91    0.00143
  | alt <= 71000  = AltitudeRegionValues 51000 270.65  (-0.0028)     66.94     0.00086
  | alt <= 86000  = AltitudeRegionValues 71000 214.65  0.002         3.96      0.000064
  | otherwise     = error "Altitude out of range, too high (>86km)"

altToPressure :: Altitude -> PressureDensity
altToPressure a@(Altitude alt) =
  let (AltitudeRegionValues hb' tb' lb' pb' rho') = altToValues a
      pr = if lb' == 0
           then pb' * exp (((- (acceleration g)) * m * (alt - hb')) / (r * tb'))
           else pb' * ((tb' / (tb' + lb' * (alt - hb')))**(((acceleration g) * m) / (r * lb')))
      dn = if lb' == 0
           then rho' * exp (((- (acceleration g)) * m * (alt - hb')) / (r * tb'))
           else rho' *
                ((tb' / (tb' + lb' * (alt - hb')))**(1 + (((acceleration g) * m) / (r * lb'))))
  in PressureDensity (Pressure pr) (Density dn)

-- | Given some number-like thing and a 'V.Vector' of other number-like things,
-- round the number-like thing to the closest thing in the 'V.Vector'.
--
-- *NOTE*: The vector must NOT be empty.
roundToClosest :: (Ord a, Num a, Integral b) => a -> V.Vector b -> b
roundToClosest n xs =
  let differences = fmap (\x -> abs (n - fromIntegral x)) xs
  in xs V.! V.minIndex differences

sim
  :: Pitch
  -> SimVals
  -> PosVel
  -> Bvars
  -> Wind
  -> V.Vector Int -- ^ Vector of pressures to round to from Grib file
  -> V.Vector GribLine
  -> Writer (D.DList Breturn) Breturn
sim pitch
    sv
    (PosVel lat' lon' alt' vel_x' vel_y' vel_z')
    (Bvars mass' bal_cd' par_cd' packages_cd' launch_time' burst_vol' b_volume' b_press')
    (Wind wind_x' wind_y')
    pressureList
    gribLines
  | baseGuard pitch = do
    let pv = PosVel lat' lon' alt' vel_x' vel_y' vel_z'
        bv = Bvars mass' bal_cd' par_cd' packages_cd' launch_time' burst_vol' b_volume' b_press'
        w = Wind windX windY
    return (Breturn sv pv bv w)
  | otherwise = do
    let sv' = sv { t = t sv + t_inc sv }
        pv = PosVel nlat nlon nAlt nvel_x nvel_y (foldPitch pitch vel_z' nvel_z)
        bv = Bvars mass' bal_cd' par_cd' packages_cd' launch_time' burst_vol' (foldPitch pitch nVol b_volume') (foldPitch pitch pres b_press')
        w = Wind windX windY
    when (round (t sv) `mod` 100 == (0 :: Integer)) $
      tell (D.singleton $ Breturn sv pv bv w)
    sim pitch sv' pv bv w pressureList gribLines
  where
    -- The guard to use depends on the pitch
    baseGuard Ascent = b_volume' >= burst_vol'
    baseGuard Descent = alt' < 0

    -- Getting pressure and density at current altitude
    PressureDensity pres dens = altToPressure alt'

    -- Calculating volume, radius, and crossectional area
    nVol = newVolume b_press' b_volume' pres
    Meter nbRad = spRadFromVol nVol
    nCAsph  = cAreaSp nbRad

    -- Calculate drag force for winds
    f_drag_x =
      case pitch of
        Ascent -> drag dens vel_x' windX bal_cd' nCAsph
        Descent -> drag dens vel_x' windX packages_cd' 1
    f_drag_y =
      case pitch of
        Ascent -> drag dens vel_y' windY bal_cd' nCAsph
        Descent -> drag dens vel_y' windY packages_cd' 1
    -- Only used for descent
    f_drag_z = drag dens vel_z' 0 par_cd' 1

    -- Net forces in z
    f_net_z = f_drag_z - (force mass' g)


    -- Calculate Kenimatics
    accel_x = accel f_drag_x mass'
    accel_y = accel f_drag_y mass'
    accel_z = accel f_net_z mass'
    nvel_x = velo vel_x' accel_x sv
    nvel_y = velo vel_y' accel_y sv
    nvel_z = velo vel_z' accel_z sv
    Altitude disp_x = displacement (Altitude 0.0) nvel_x accel_x sv
    Altitude disp_y = displacement (Altitude 0.0) nvel_y accel_y sv
    nAlt = displacement alt' vel_z' 0.0 sv

    -- Calculate change in corrdinates
    -- Because of the relatively small changes, we assume a spherical earth
    bearing = atan2 disp_y disp_x
    t_disp = (disp_x ** 2 + disp_y ** 2) ** (1 / 2)
    ang_dist = t_disp / er
    
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

    filterPressure = roundToClosest pres pressureList
    (windX, windY) =
      case filterGrib lat' lon' filterPressure [UGRD, VGRD] gribLines of
        Just (GribPair (UGRDLine u) (VGRDLine v)) ->
          (WindMs (velocity u), WindMs (velocity v))
        Nothing -> (wind_x', wind_y')
