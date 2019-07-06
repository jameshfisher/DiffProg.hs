module Eval where

import Types
import Helpers

import qualified Data.Vector as V
import Data.Vector ((!), (//))

evalExpr :: V.Vector Value -> Expr -> Value
evalExpr values expr = case expr of
  ExprConst d -> d
  ExprAdd indexes -> sum $ map (values !) indexes
  ExprNeg i1 -> -(values ! i1)
  ExprMul i1 i2 -> (values ! i1) * (values ! i2)
  ExprReLU i1 -> let r = values ! i1 in if r < 0 then 0 else r
  ExprLogistic i1 -> logistic $ values!i1

evalProg :: Prog -> V.Vector Value
evalProg (Prog prog) = V.foldl' f V.empty prog where
  f vals expr = V.snoc vals $ evalExpr vals expr
