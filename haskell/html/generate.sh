#!/usr/bin/env bash
set -e

if [[ "$1" == "" ]] || [[ ! -f "$1" ]]; then
  echo "Argument to script must be a Grib2 CSV file."
  exit 1
fi

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
pushd $DIR/..
  cabal build
  echo "*** Running habsim-jsdump"
  dist/build/habsim-jsdump/habsim-jsdump "$1" > $DIR/flight_path.js
  echo "Open $DIR/index.html in your browser."
popd
