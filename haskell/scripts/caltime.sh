#!/usr/bin/env bash

# This calculates which GFS GRIB future file to download for a launch

# Launch Time
lyear=2017      # Launch Year
lmth=06         # Launch Month
lday=22         # Launch Day
lhr=07          # Launch Hour
lepoch=$(date -u --date="$lyear$lmth$lday $lhr" +%s)

#Current Time
cyear=$(date -u +%Y) 
cmth=$(date -u +%m) 
cday=$(date -u +%d) 
chr=$(date -u +%H)
cepoch=$(date -u --date="$cyear$cmth$cday $chr" +%s)

#Sim Time
syear=$cyear
smth=$cmth
sday=$cday
shr=$chr

echo " "
echo "You've indicated a launch on $lyear $lmth $lday at $lhr UTC."

# Calculate the last sim time
if [ $chr -lt 06 ]; then
  shr=00
elif [ $chr -lt 12 ]; then
  shr=06
elif [ $chr -lt 18 ]; then
  shr=12
elif [ $chr -lt 24 ]; then
  shr=18
else
  echo "Unexpected error with calculating sim time..."
fi

# Calculate difference from sim to launch in hours
sepoch=$(date -u --date="$syear$smth$sday $shr" +%s)
dsepoch=$((lepoch-sepoch))
dshrs=$(echo "$dsepoch/60/60" | bc)

if [ $dshrs -gt 168 ]; then
  echo "Please choose a launch time less than a week away"
fi

echo "Last sim was on $syear $smth $sday at $shr UTC" 
echo " "

flout=$(printf "%03d" $dshrs)
echo "File to download is f$flout"
echo " "



