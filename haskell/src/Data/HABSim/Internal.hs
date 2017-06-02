module Data.HABSim.Internal where

import Control.Lens
import Data.HABSim.Lens
import Data.HABSim.Types
import qualified Data.Vector as V

-- | Constants
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
{-# INLINE newVolume #-}

-- | Calculate sphereical radius from volume
spRadFromVol :: Volume -> SphericalRadius
spRadFromVol (Liter v) = Meter $ ((3 * v) / (4 * pi)) ** (1 / 3)
{-# INLINE spRadFromVol #-}

-- Calculate cross sectional area of sphere.
cAreaSp :: Double -> CrossSecArea
cAreaSp r = CrossSecArea $ pi * (r ** 2)
{-# INLINE cAreaSp #-}

-- Calculate gas density given molar mass, temp and pressure.
gas_dens :: Double -> Double -> Double -> Double
gas_dens mm p t = (mm*p)/(r*t)
{-# INLINE gas_dens #-}

-- | Calculate boyancy.
boyancy :: Double -> Double -> Double -> Double
boyancy p_air p_gas v = (p_air-p_gas)*(g ^. acceleration)*v
{-# INLINE boyancy #-}

-- Calculate drag.
drag :: Density -> Velocity -> WindMs -> CoeffDrag -> CrossSecArea -> Force
drag (Density d) (Velocity v) (WindMs w) (CoeffDrag c) (CrossSecArea a) =
  Force $ (1 / 2) * d * ((if v < w then 1 else (-1)) * ((v - w) ** 2)) * c * a
{-# INLINE drag #-}

-- Calculate acceleration.
accel :: Force -> Mass -> Acceleration
accel (Force f) (Mass m) = Acceleration $ f / m
{-# INLINE accel #-}

force :: Mass -> Acceleration -> Force
force (Mass m) (Acceleration a) = Force $ m * a
{-# INLINE force #-}

-- Calculate velocity.
velo :: Velocity -> Acceleration -> SimulationTime -> Velocity
velo (Velocity v) (Acceleration a) (SimulationTime t _) = Velocity $ v + a * t
{-# INLINE velo #-}

-- Calculate displacement.
displacement
  :: Altitude
  -> Velocity
  -> Acceleration
  -> SimulationTime
  -> Altitude
displacement (Altitude x) (Velocity v) (Acceleration a) (SimulationTime t _) =
  Altitude $ x + (v * t) + ((1 / 2) * a * t ** 2)
{-# INLINE displacement #-}

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
           then pb' * exp (((- (g ^. acceleration)) * m * (alt - hb')) / (r * tb'))
           else pb' * ((tb' / (tb' + lb' * (alt - hb')))**(((g ^. acceleration) * m) / (r * lb')))
      dn = if lb' == 0
           then rho' * exp (((- (g ^. acceleration)) * m * (alt - hb')) / (r * tb'))
           else rho' *
                ((tb' / (tb' + lb' * (alt - hb')))**(1 + (((g ^. acceleration) * m) / (r * lb'))))
  in PressureDensity (Pressure pr) (Density dn)

-- | Given some number-like thing and a 'V.Vector' of other number-like things,
-- round the number-like thing to the closest thing in the 'V.Vector'.
--
-- *NOTE*: The vector must NOT be empty.
--
-- We should some day generalize this (Lens?), it seems useful.
roundToClosest :: (Ord a, Num a, Integral b) => a -> V.Vector b -> b
roundToClosest n xs =
  let differences = fmap (\x -> abs (n - fromIntegral x)) xs
  in xs V.! V.minIndex differences
