# habsim
High Altitude Balloon Simulator

This is the beginning of a high altitude balloon simulator.

Current functionality:

* Calculates vertical ascent and decent of a balloon.

Considers:

* ISA Atmospheric Model on ascent and decent.
* Calculates changing drag from balloon envlope and atmospheric density on ascent
* Calculates drag from parachute (given a Cd)

To Do: (roughly in order of importance)

* Make the code cleaner breaking things out into functions.
* Temperature effects of atmosphere on balloon during ascent.
* Config file, or command line input, of parameters
* Longitude and latitude calculations from wind data.
