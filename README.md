# habsim
High Altitude Balloon Simulator

This is the beginning of a high altitude balloon simulator. 

Currently Ricky and I (Jimmy) are working on the haskell version of this simulator. We have a demo program which plots the results of these sims. It's not polished right now but it works and its a start. I plan on updating this readme soon.

Considers:

* ISA Atmospheric Model on ascent and decent.
* Calculates changing drag from balloon envlope and atmospheric density on ascent
* Calculates drag from parachute (given a Cd)

To Do: (roughly in order of importance)

* Make the code cleaner breaking things out into functions.
* Temperature effects of atmosphere on balloon during ascent.
* Config file, or command line input, of parameters
* Longitude and latitude calculations from wind data.

URL Used to get GRIB data

http://nomads.ncep.noaa.gov/cgi-bin/filter_gfs_0p25.pl?file=gfs.t18z.pgrb2.0p25.anl&lev_1_mb=on&lev_1000_mb=on&lev_100_mb=on&lev_10_mb=on&lev_150_mb=on&lev_2_mb=on&lev_200_mb=on&lev_20_mb=on&lev_250_mb=on&lev_3_mb=on&lev_300_mb=on&lev_30_mb=on&lev_350_mb=on&lev_400_mb=on&lev_450_mb=on&lev_5_mb=on&lev_500_mb=on&lev_50_mb=on&lev_550_mb=on&lev_600_mb=on&lev_650_mb=on&lev_7_mb=on&lev_700_mb=on&lev_70_mb=on&lev_750_mb=on&lev_800_mb=on&lev_850_mb=on&lev_900_mb=on&lev_925_mb=on&lev_950_mb=on&lev_975_mb=on&var_TMP=on&var_UGRD=on&var_VGRD=on&subregion=&leftlon=-81&rightlon=-80&toplat=41&bottomlat=40&dir=%2Fgfs.2017050818
