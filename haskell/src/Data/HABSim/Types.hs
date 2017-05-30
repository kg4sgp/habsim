{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.HABSim.Types where

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

data Simulation =
  Simulation { retSV :: SimVals
             , retPV :: PosVel
             , retBV :: Bvars
             , retW  :: Wind
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
