<img
  src="https://tmp.elrod.me/habsim.svg"
  alt="HABSim">

High Altitude Balloon Simulator

## Currently Considers:

* ISA Atmospheric Model on ascent and decent.
* Calculates changing drag from balloon envlope and atmospheric density on
  ascent.
* Calculates drag from parachute (given a coefficient of drag).

## Planned (roughly in order of importance):

* Make the code cleaner breaking things out into functions.
* Temperature effects of atmosphere on balloon during ascent.
* Config file, or command line input, of parameters.
* Longitude and latitude calculations from wind data.

### Codebases

There are two implementations, one in `haskell/` and one in `c/`.
Haddock for the Haskell library can be found
[here](https://kg4sgp.github.io/habsim/)
