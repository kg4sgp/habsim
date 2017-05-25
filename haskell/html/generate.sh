#!/usr/bin/env bash
set -e
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
pushd $DIR/..
  cabal build
  dist/build/habsim/habsim > $DIR/flight_path.js
  echo "Open $DIR/index.html in your browser."
popd
