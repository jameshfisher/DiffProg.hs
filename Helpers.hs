module Helpers where

import qualified Data.Vector as V
import Data.Vector ((!), (//))

-- why does this not exist
vSet :: Int -> a -> V.Vector a -> V.Vector a
vSet i x v = v // [(i, x)]

indexesOf :: V.Vector a -> [Int]
indexesOf v = [0 .. length v - 1]

logistic :: Double -> Double
logistic x = 1 / (1 + exp (-x))