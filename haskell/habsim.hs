module Main where

newtype Pascal = Pascal Double deriving (Eq, Ord, Show)
newtype Meter = Meter Double deriving (Eq, Ord, Show)
newtype Celsius = Celsius Double deriving (Eq, Ord, Show)
newtype M3 = M3 Double deriving (Eq, Ord, Show)
newtype Second = Second Double deriving (Eq, Ord, Show)
newtype Altitude = Altitude Double deriving (Eq, Ord, Show)
newtype Latitude  = Latitude Double deriving (Eq, Ord, Show)
newtype Longitude = Longitude Double deriving (Eq, Ord, Show)
newtype Pressure = Pressure Double deriving (Eq, Ord, Show)
newtype Density = Density Double deriving (Eq, Ord, Show)
--newtype kg = kg deriving (Eq, Ord, Show)
--newtype MolarMass = MolarMass deriving (Eq, Ord, Show)

data Coordinate = Coordinate Latitude Longitude Altitude deriving (Show, Eq, Ord)

data AzElCord =
  AzelCord { azimuth    :: Double
           , elevation  :: Double
           , range      :: Double 
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
        , b_pres        :: !Double
        } deriving (Eq, Ord, Show)
        
data Wind =
  Wind { velo_x         :: !Double
       , velo_y         :: !Double
       } deriving (Eq, Ord, Show)
       
data Breturn = Breturn SimVals PosVel Bvars Wind  deriving (Eq, Ord, Show)

-- | constants
m, r, g, er :: Double
m   = 0.0289644
r   = 8.3144598
g   = 9.80665
er  = 6378137.0

-- | Calculate new volume given an initial pressure and volume, and a new
-- pressure.
newVolume :: Fractional a => a -> a -> a -> a
newVolume p v new_p = (p*v)/new_p

-- | Calculate sphereical radius from volume
spRadFromVol :: Floating a => a -> a
spRadFromVol v = ((3*v)/(4*pi))**(1/3)

-- Calculate cross sectional area of sphere.
cAreaSp :: Floating a => a -> a
cAreaSp r = pi*(r**2)

-- Calculate gas density given molar mass, temp and pressure.
gas_dens :: Double -> Double -> Double -> Double
gas_dens mm p t = (mm*p)/(r*t)

-- | Calculate boyancy.
boyancy :: Double -> Double -> Double -> Double
boyancy p_air p_gas v = (p_air-p_gas)*g*v

-- Calculate drag.
drag :: Floating a => a -> a -> a -> a -> a -> a
drag den v flv cd a = (1/2)*den*((v-flv)**2)*cd*a

-- Calculate acceleration.
accel :: Fractional a => a -> a -> a
accel f m = f/m

-- Calculate velocity.
velo :: Num a => a -> a -> a -> a
velo v a t = v + a*t

-- Calculate displacement.
displacement :: Floating a => a -> a -> a -> a -> a
displacement x v a t = x + (v*t) + ((1/2)*a*t**2)

    

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
           then pb' * exp (((-g) * m * (alt - hb')) / (r * tb'))
           else pb' * ((tb' / (tb' + lb' * (alt - hb')))**((g * m) / (r * lb')))
      dn = if lb' == 0
           then rho' * exp (((-g) * m * (alt - hb')) / (r * tb'))
           else rho' *
                ((tb' / (tb' + lb' * (alt - hb')))**(1 + ((g * m) / (r * lb'))))
  in PressureDensity (Pressure pr) (Density dn)

main = print (sim (SimVals 0.01 0.0) (PosVel 0.0 0.0 0.0 0.0 0.0 3.0) (Bvars 2.0 0.47 1.0 0.5 0.0 540.0 5.0 120000.0) (Wind 4.0 4.0))

sim :: SimVals -> PosVel -> Bvars -> Wind -> Breturn
sim (SimVals t_inc' t')
    (PosVel lat' lon' alt' vel_x' vel_y' vel_z') 
    (Bvars mass' bal_cd' par_cd' packages_cd' launch_time' burst_vol' b_volume' b_press')
    (Wind wind_x' wind_y')
  -- if the burst volume has been reached print the values
  -- otherwise tail recurse with the new updated values
  | b_volume' >= burst_vol' = Breturn (SimVals t_inc' t') (PosVel lat' lon' alt' vel_x' vel_y' vel_z') (Bvars mass' bal_cd' par_cd' packages_cd' launch_time' burst_vol' b_volume' b_press') (Wind wind_x' wind_y')
  | otherwise = sim (SimVals t_inc' (t'+t_inc')) (PosVel nlat nlon nAlt nvel_x nvel_y vel_z') (Bvars mass' bal_cd' par_cd' packages_cd' launch_time' burst_vol' nVol pres) (Wind wind_x' wind_y')
  where
    -- Getting pressure and density at current altitude
    (PressureDensity (Pressure pres) (Density dens)) = altToPressure (Altitude alt')
    
    -- Calculating volume, radius, and crossectional area
    nVol    = newVolume b_press' b_volume' pres
    nb_rad  = spRadFromVol nVol
    nCAsph  = cAreaSp nb_rad
    
    -- Calculate drag force for winds
    f_drag_x = drag dens vel_x' wind_x' bal_cd' nCAsph
    f_drag_y = drag dens vel_y' wind_y' bal_cd' nCAsph
    
    -- Calculate Kenimatics
    accel_x = accel f_drag_x mass'
    accel_y = accel f_drag_y mass'
    nvel_x = velo vel_x' accel_x t_inc'
    nvel_y = velo vel_y' accel_y t_inc'
    disp_x = displacement 0.0 nvel_x accel_x t_inc'
    disp_y = displacement 0.0 nvel_y accel_y t_inc'
    nAlt = displacement alt' vel_z' 0.0 t_inc'
    
    -- Calculate change in corrdinates
    -- Because of the relatively small changes, we assume a spherical earth
    drlat = (disp_y / (er + alt'))
    drlon = (disp_x / (er + alt'))
    dlat = drlat*(180/pi)
    dlon = drlon*(180/pi)
    nlat = lat' + nlat
    nlon = lon' + nlon
  
  -- find the density and pressurefrom altitude
  --f_drag_x = drag den 
            




