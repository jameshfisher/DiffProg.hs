module DiffProg where

import Types
import Eval
import Test (poorManBackProp)
import BackProp
import Descent

import Data.Maybe (fromJust)
import qualified Data.Vector as V
import Data.Vector ((!), (//))

import Test.QuickCheck (quickCheck, generate)

main = do
  let prog = Prog $ V.fromList [ExprConst (-8.412612524740386), ExprMul 0 0]
  -- prog <- generate arbitraryProg
  let minimized = minimize [0] prog
  -- let values = evalProg prog
  -- let derivs = backProp prog values
  -- let poorDerivs = poorManBackProp prog values
  -- print prog
  -- print derivs
  -- print poorDerivs
  print minimized