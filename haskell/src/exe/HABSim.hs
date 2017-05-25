module Main where

import Control.Monad.Writer
import Data.HABSim.HABSim
import qualified Data.DList as D
import Data.Foldable (traverse_)

main :: IO ()
main = do
  let sv = SimVals 0.1 0.0
      pv = PosVel 0.0 0.0 0.0 0.0 0.0 3.0
      bv = Bvars 2.0 0.47 1.0 0.5 0.0 540.0 (Liter 5.0) 120000.0
      w = Wind 4.0 4.0
      (lastAscent@(Breturn sv' pv' bv' w'), accAscent) =
        runWriter $ sim Ascent sv pv bv w
      (lastDescent, accDescent) =
        runWriter $ sim Descent sv' pv' bv' w'
  traverse_ print (D.cons lastAscent accAscent)
  traverse_ print (D.cons lastDescent accDescent)

