#!/usr/bin/env bash

ret=0

# This ensures our debug program is still streaming output
#timeout 5 ./dist/build/habsim-debug/habsim-debug /tmp/2017052818-big.csv | head > /tmp/s
#
#if [[ "$(stat --printf="%s" /tmp/s)" == "0" ]]; then
#  echo "streaming output seems broken."
#  ret=1
#fi

#echo "$ cat /tmp/s"
#cat /tmp/s

echo "Testing HTML generation"
./html/generate.sh /tmp/2017052818-big.csv
if [[ $(wc -l <html/flight_path.js) -le 30 ]]; then
  cat html/flight_path.js
  echo "flight_path.js is less than 30 lines."
  ret=1
fi

exit $ret
