module Main where

newtype Pascal = Pascal Double deriving (Eq, Ord, Show)
newtype Meter = Meter Double deriving (Eq, Ord, Show)
newtype Celsius = Celsius Double deriving (Eq, Ord, Show)
newtype M3 = M3 Double deriving (Eq, Ord, Show)
newtype Second = Second Double deriving (Eq, Ord, Show)
newtype Altitude = Altitude Double deriving (Eq, Ord, Show)
newtype Pressure = Pressure Double deriving (Eq, Ord, Show)
newtype Density = Density Double deriving (Eq, Ord, Show)
--newtype kg = kg deriving (Eq, Ord, Show)
--newtype MolarMass = MolarMass deriving (Eq, Ord, Show)

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
          , ascent_r  :: !Double
          , alti      :: !Double
          } deriving (Eq, Ord, Show)

data PosVel = 
  PosVel {  lat       :: !Double
          , lon       :: !Double
          , alt       :: !Double
          , vel_x     :: !Double
          , vel_y     :: !Double
          , vel_z     :: !Double
          } deriving (Eq, Ord, Show)
          
data Bvars =
  Bvars { mass          :: !Double
        , bal_cd        :: !Double
        , par_cd        :: !Double
        , packages_cd   :: !Double
        , launch_time   :: !Double
        , burst_vol     :: !Double
        , b_vol         :: !Double
        } deriving (Eq, Ord, Show)
        
data Wind =
  Wind { velo_x         :: !Double
       , velo_y         :: !Double
       } deriving (Eq, Ord, Show)
       
data Breturn = Breturn PosVel Bvars Wind

-- | constants
m, r, g :: Double
m = 0.0289644
r = 8.3144598
g = 9.80665

--calculate new volume given an initial pressure and volume, and a new pressrure
newVolume p v new_p = (p*v)/new_p

-- calculate sphereical radius from volume
spRadFromVol v = ((3*v)/(4*pi))**(1/3)

-- calculate cross sectional area of sphere
cAreaSp r = pi*(r**2)

-- calculate gas density given molar mass, temp and pressure
gas_dens mm p t = (mm*p)/(r*t)

-- | calculate boyancy
boyancy p_air p_gas v = (p_air-p_gas)*g*v

-- calculate drag
drag den v flv cd a = (1/2)*den*((v-flv)**2)*cd*a

-- calculate acceleration
acel f m = f/m

-- calculate velocity
velo v a t = v + a*t

-- calculate displacement
displacement x v a t = x + (v*t) + ((1/2)*a*t**2)  

altToValues :: Altitude -> AltitudeRegionValues
altToValues (Altitude alt)
  | alt < 11000 = AltitudeRegionValues  0     288.15 (-0.0065)     101325  1.225
  | alt <= 20000 = AltitudeRegionValues 11000 216.65 0             22632.1 0.36391
  | alt <= 32000 = AltitudeRegionValues 20000 216.65 0.001         5474.89 0.08803
  | alt <= 47000 = AltitudeRegionValues 32000 228.65 0.0028        868.02  0.01322
  | alt <= 51000 = AltitudeRegionValues 47000 270.65 0             110.91  0.00143
  | alt <= 71000 = AltitudeRegionValues 51000 270.65 (-0.0028)     66.94   0.00086
  | alt <= 86000 = AltitudeRegionValues 71000 214.65 0.002         3.96    0.000064
  | otherwise = error "Altitude out of range (>86km)"
  
altToPressure :: Altitude -> PressureDensity
altToPressure a@(Altitude alt) =
  let (AltitudeRegionValues hb' tb' lb' pb' rho') = altToValues a
      pr = if lb' == 0
           then pb' * exp (((-g) * m * (alt - hb')) / (r * tb'))
           else pb' * ((tb' / (tb' + lb' * (alt - hb')))**((g * m) / (r * lb')))
      dn = if lb' == 0
           then rho' * exp (((-g) * m * (alt - hb')) / (r * tb'))
           else rho' *
                ((tb' / (tb' + lb' * (alt - hb')))**(1 + ((g * m) / (r * lb'))))
  in PressureDensity (Pressure pr) (Density dn)

--sim :: PosVel -> Bvars -> Wind -> Breturn
--sim (PosVel lat' lon' atl' vel_x' vel_y' vel_z') 
--    (Bvar mass' bal_cd' par_cd' packages_cd' launch_time' burst_dia' b_volume')
--    (Wind
--  | 
--  where
  
  
  -- find the density and pressurefrom altitude
  --f_drag_x = drag den 
            




