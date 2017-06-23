module Data.HABSim.VectorUtilities where

import qualified Data.Set as S
import qualified Data.Vector as V

-- Semi-efficient vector nub
nub :: (Ord a) => V.Vector a -> V.Vector a
nub = f S.empty
  where
    f s vec
      | V.null vec = V.empty
      | S.member (V.head vec) s = f s (V.tail vec)
      | otherwise =
          V.cons (V.head vec) (f (S.insert (V.head vec) s) (V.tail vec))

