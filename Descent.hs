module Descent where

import qualified Data.Vector as V
import Data.Vector ((!), (//))

import Types
import Eval
import BackProp

step :: Double -> [Index] -> Prog -> Prog
step learningRate weightIndexes prog@(Prog exprs) = Prog newExprs where
  values = evalProg prog
  derivs = backProp prog values
  updates :: [(Index, Expr)]
  updates = map (\i -> (i, newAtIndex i)) weightIndexes
  newAtIndex :: Index -> Expr
  newAtIndex i = case exprs!i of
    ExprConst c -> ExprConst $ c - (derivs!i * learningRate)
  newExprs = exprs // updates

minimize :: [Index] -> Prog -> Prog
minimize weightIndexes prog = (iterate (step 0.1 weightIndexes) prog) !! 100
