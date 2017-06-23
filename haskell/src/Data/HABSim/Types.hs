{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.HABSim.Types where

import Data.Csv (FromField)
import Data.Hashable (Hashable)

#define DoubleGND Enum, Eq, Floating, Fractional, FromField, Hashable, Num, \
  Ord, Read, Real, RealFloat, RealFrac, Show

-- I'd like to see us get rid of these and maybe use something like the
-- @dimensional@ package instead. We can still wrap them in newtypes, but we'll
-- get extra type-safety this way, at the cost of possibly some syntactical
-- annoyance. The trade-off seems worth it to me.
newtype Meter = Meter { _meter :: Double } deriving (DoubleGND)
newtype Altitude = Altitude { _altitude :: Double } deriving (DoubleGND)
newtype Latitude  = Latitude { _latitude :: Double } deriving (DoubleGND)
newtype Longitude = Longitude { _longitude :: Double } deriving (DoubleGND)
newtype Pressure = Pressure { _pressure :: Double } deriving (DoubleGND)
newtype Density = Density { _density :: Double } deriving (DoubleGND)
newtype Liter = Liter { _liter :: Double } deriving (DoubleGND)
newtype WindMs = WindMs { _windMs :: Double } deriving (DoubleGND)
newtype CoeffDrag = CoeffDrag { _coeffDrag :: Double } deriving (DoubleGND)
newtype Velocity = Velocity { _velocity :: Double } deriving (DoubleGND)
newtype CrossSecArea =
  CrossSecArea { _crossSecArea :: Double } deriving (DoubleGND)
newtype Force = Force { _force :: Double } deriving (DoubleGND)
newtype Acceleration =
  Acceleration { _acceleration :: Double } deriving (DoubleGND)
newtype Mass = Mass { _mass :: Double } deriving (DoubleGND)
newtype Displacement =
  Displacement { _displacement :: Double } deriving (DoubleGND)
newtype WindX = WindX { _windX :: WindMs } deriving (DoubleGND)
newtype WindY = WindY { _windY :: WindMs } deriving (DoubleGND)
newtype Temp = Temp { _temp :: Double } deriving (DoubleGND)

-- Useful aliases
type Volume = Liter
type SphericalRadius = Meter

data Coordinate =
  Coordinate { _lat :: Latitude
             , _lon :: Longitude
             , _alt :: Altitude } deriving (Show, Eq, Ord)

data AzElCord =
  AzelCord { _azimuth    :: !Double
           , _elevation  :: !Double
           , _range      :: !Double
           }

data PressureDensity =
  PressureDensity { _pressureDensityPressure :: Pressure
                  , _pressureDensityDensity :: Density
                  , _pressureDensityTemp    :: Temp
                  } deriving (Eq, Ord, Show)

data AltitudeRegionValues =
  AltitudeRegionValues { _hb  :: !Double
                       , _tb  :: !Double
                       , _lb  :: !Double
                       , _pb  :: !Double
                       , _rho :: !Double
                       , _tmp :: !Double
                       } deriving (Eq, Ord, Show)

data SimulationTime =
  SimulationTime { _increment :: !Double
                 , _simulationTime :: !Double
                 } deriving (Eq, Ord, Show)

data PosVel =
  PosVel {  _posVelLat       :: !Double
          , _posVelLon       :: !Double
          , _posVelAlt       :: Altitude
          , _posVelVel_x     :: Velocity
          , _posVelVel_y     :: Velocity
          , _posVelVel_z     :: Velocity
          } deriving (Eq, Ord, Show)

data Burst =
  Burst { _burstMass          :: Mass
        , _burstBal_cd        :: CoeffDrag
        , _burstPar_cd        :: CoeffDrag
        , _burstPackages_cd   :: CoeffDrag
        , _burstLaunch_time   :: !Double
        , _burstBurst_vol     :: Volume
        , _burstB_vol         :: Volume
        , _burstB_pres        :: Pressure
        , _burstMolMass       :: !Double
        , _burstTemp          :: !Double
        } deriving (Eq, Ord, Show)

data Wind =
  Wind { _velo_x         :: WindX
       , _velo_y         :: WindY
       } deriving (Eq, Ord, Show)

data Simulation =
  Simulation { _retSV :: SimulationTime
             , _retPV :: PosVel
             , _retBV :: Burst
             , _retW  :: Wind
             } deriving (Eq, Ord, Show)

data Pitch = Ascent | Descent deriving (Eq, Show)

-- | Case analysis for 'Pitch'.
pitch
  :: Pitch -- ^ The pitch we are analyzing
  -> a -- ^ What to return if it is 'Ascent'
  -> a -- ^ What to return if it is 'Descent'
  -> a -- ^ The thing we decide to return
pitch Ascent a _ = a
pitch Descent _ b = b
