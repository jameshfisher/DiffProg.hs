module DiffProg where

import Types
import Eval
import Test (poorManBackProp)
import BackProp
import Descent
import NeuralNet
import Graphviz

import Data.Maybe (fromJust)
import qualified Data.Vector as V
import Data.Vector ((!), (//))

import Test.QuickCheck (quickCheck, generate)

-- -- Show an example of back prop
-- main = do
--   prog <- generate arbitraryProg
--   let values = evalProg prog
--   let derivs = backProp prog values
--   let poorDerivs = poorManBackProp prog values
--   print prog
--   print derivs
--   print poorDerivs

-- -- minimize (w+2)^2
-- main = do
--   let prog = Prog $ V.fromList [ExprConst (-10), ExprConst 2, ExprAdd [0, 1], ExprMul 2 2]
--   let minimized = minimize [0] prog
--   print minimized

main = do
  let (prog0, indexes0) = (Prog $ V.fromList [ExprConst 13, ExprConst 14], [0, 1])
  let (prog1, indexes1) = addNeuronLayerToProg indexes0 2 prog0
  let (prog2, indexes2) = addNeuronLayerToProg indexes1 1 prog1
  writeDot prog2