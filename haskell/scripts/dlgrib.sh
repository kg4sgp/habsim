#!/usr/bin/env bash

# This is to help download and prepare a grib file for simulation
# Calls this script with
# ./dlgrib Lat Lon +-deg YYYY MM DD HH FF

#### Time of Sim ####
# choose 00, 06, 12, 18
ysim="$4"
msim="$5"
dsim="$6"
simtime="$7"

#### Balloon Prediction time ###
# ahead of sim time in hours, 3 digitis
# if 0-120 increments of 1
# if 120-240 increments of 3
# if 240-384 increments of 12
predictime="$8"

#### Location ####
# The data we are getting is in precision of 0.25 best to stick with those
lat="$1"
lon="$2"
area="$3"
t_lat=$(($lat + $area))
b_lat=$(($lat - $area))
l_lon=$(($lon - $area))
r_lon=$(($lon + $area))

# download directory
dldir=~/Downloads/gfs-grib


echo " "
echo "Making $dldir and subdirectories if they dont exsist..."
mkdir -p $dldir/raw
mkdir -p $dldir/csv


echo " "
echo "Downloading from http://nomads.ncep.noaa.gov ..."
curl "http://nomads.ncep.noaa.gov/cgi-bin/filter_gfs_0p25.pl?\
file=gfs.t${simtime}z.pgrb2.0p25.f${predictime}&\
lev_1_mb=on&lev_1000_mb=on&lev_100_mb=on&lev_10_mb=on&lev_150_mb=on&\
lev_2_mb=on&lev_200_mb=on&lev_20_mb=on&lev_250_mb=on&lev_3_mb=on&\
lev_300_mb=on&lev_30_mb=on&lev_350_mb=on&lev_400_mb=on&lev_450_mb=on&\
lev_5_mb=on&lev_500_mb=on&lev_50_mb=on&lev_550_mb=on&lev_600_mb=on&\
lev_650_mb=on&lev_7_mb=on&lev_700_mb=on&lev_70_mb=on&lev_750_mb=on&\
lev_800_mb=on&lev_850_mb=on&lev_900_mb=on&lev_925_mb=on&lev_950_mb=on&\
lev_975_mb=on&var_TMP=on&var_UGRD=on&var_VGRD=on&subregion=&leftlon=${l_lon}&\
rightlon=${r_lon}&toplat=${t_lat}&bottomlat=${b_lat}&\
dir=%2Fgfs.$ysim$msim$dsim$simtime" -o $dldir/raw/$ysim$msim$dsim$simtime-f${predictime}.gfs


echo " "
echo "Converting GFS GRIB file to CSV..."
echo " "
wgrib2 $dldir/raw/$ysim$msim$dsim$simtime-f${predictime}.gfs -csv $dldir/csv/$ysim$msim$dsim$simtime-f${predictime}.csv

echo " "
echo "In the habsim/haskell directory build and then run:"
echo "./html/generate.sh ${dldir}/csv/$ysim$msim$dsim$simtime-f${predictime}.csv"

echo " "
echo "Running simulation..."
sh ../html/generate.sh ${dldir}/csv/$ysim$msim$dsim$simtime-f${predictime}.csv

 
echo " "
echo "Done!"
echo " "
