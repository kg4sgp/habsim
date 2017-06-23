{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.HABSim.Lens where

import Control.Lens
import qualified Data.HABSim.Types as HT
import qualified Data.HABSim.Grib2.CSVParse.Types as CT

-- Data.HABSim.Types
makeFieldsNoPrefix ''HT.Acceleration
makeFieldsNoPrefix ''HT.Altitude
makeFieldsNoPrefix ''HT.AltitudeRegionValues
makeFieldsNoPrefix ''HT.AzElCord
makeFields         ''HT.Burst
makeFieldsNoPrefix ''HT.CoeffDrag
makeFieldsNoPrefix ''HT.Coordinate
makeFieldsNoPrefix ''HT.CrossSecArea
makeFieldsNoPrefix ''HT.Density
makeFieldsNoPrefix ''HT.Displacement
makeFieldsNoPrefix ''HT.Force
makeFieldsNoPrefix ''HT.Latitude
makeFieldsNoPrefix ''HT.Liter
makeFieldsNoPrefix ''HT.Longitude
makeFieldsNoPrefix ''HT.Mass
makeFieldsNoPrefix ''HT.Meter
makeFields         ''HT.PosVel
makeFieldsNoPrefix ''HT.Pressure
makeFields         ''HT.PressureDensity
makeFieldsNoPrefix ''HT.Simulation
makeFieldsNoPrefix ''HT.SimulationTime
makeFieldsNoPrefix ''HT.Velocity
makeFieldsNoPrefix ''HT.Wind
makeFieldsNoPrefix ''HT.WindMs
makeFieldsNoPrefix ''HT.WindX
makeFieldsNoPrefix ''HT.WindY
makeFieldsNoPrefix ''HT.Temp

-- Data.HABSim.Grib2.CSVParse.Types
makeFieldsNoPrefix ''CT.GribTime
makeFieldsNoPrefix ''CT.RawGribLine
makeFields         ''CT.UGRDLine
makeFields         ''CT.VGRDLine
