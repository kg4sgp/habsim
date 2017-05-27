#!/usr/bin/env bash

cwd="$( cd "${BASH_SOURCE[0]%/*}" && pwd )"
cd "$cwd/.."
f=`mktemp -d`
git clone "git@github.com:kg4sgp/habsim.git" "$f/habsim.git"
cabal haddock
pushd "$f/habsim.git"
  git checkout gh-pages && git rm -rf *
popd
mv dist/doc/html/habsim/* "$f/habsim.git/"
pushd "$f/habsim.git"
  git add -A
  git commit -m "Manual docs deploy."
  git push origin gh-pages
popd
rm -rf "$f"

if [ $? == 0 ]; then
  echo "*** Done: https://kg4sgp.github.io/habsim/"
  exit 0
else
  echo "*** ERROR!!! Fix the above and try again."
  exit 1
fi
