<img
  src="https://tmp.elrod.me/habsim.svg"
  alt="HABSim">

HABSim - A high altitude balloon simulator.

This program numerically simulates the flight path of a high altitude balloon.
HABSim is meant to be a simulation engine which outputs simulation data for
other programs to use and display.

We aim to have this program as data agnostic as possible. Currently NOEXC uses
data from
[NOAA's Global Forecast System](https://www.ncdc.noaa.gov/data-access/model-data/model-datasets/global-forcast-system-gfs)
to make flight predictions.

## Currently Considers:

* ISA Atmospheric Model - Used to determine pressure, density and temperature
  of the atmosphere.
* Constant ascent rate - for simplicity currently. Calculated ascent rate soon.
* Calculated lateral velocity - Lateral velocity from drag on balloon due to
  wind.
* Calculated descent velocity - calculates drag from parachute (given a
  coefficient of drag).
* Calculated burst altitude - given burst volume and initial fill volume.
* Interpolated wind - bilinear interpolation of wind between lat/lon gird
  points.

## Planned (roughly in order of importance):

* Calculated ascent rate - use combined gas law. (ascent rate model
  improvement)
* Balloon Thermodynamics - energy input into balloon system.  (ascent rate
  model improvement)

### Codebases

There are two implementations, one in `haskell/` and one in `c/`. The haskell
codebase is generally more full featured, better maintained, and is the
intended simulator to use for flight purposes. Haddock for the Haskell library
can be found [here](https://kg4sgp.github.io/habsim/).
