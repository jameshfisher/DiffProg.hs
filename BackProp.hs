module BackProp where

import Types
import Helpers

import qualified Data.Vector as V
import Data.Vector ((!), (//))

backPropExpr :: Prog -> V.Vector Value -> Index -> [(Index, Derivative)]
backPropExpr prog@(Prog exprs) values i = case exprs ! i of
  ExprConst c -> []
  ExprAdd is -> map (\i -> (i, 1)) is
  ExprNeg i -> [(i, -1)]
  ExprMul i1 i2 -> [(i1, values!i2), (i2, values!i1)]
  ExprReLU i -> [(i, if values ! i < 0 then 0 else 1)]
  ExprLogistic i -> let l = logistic (values!i) in [(i, l * (1-l))]

-- assumes derivs!i is complete
backPropAt :: Prog -> V.Vector Value -> Index -> V.Vector Derivative -> V.Vector Derivative
backPropAt prog@(Prog exprs) values i derivs = V.accum updateDeriv derivs updates where
  updates = backPropExpr prog values i
  updateDeriv deriv newDeriv = deriv + newDeriv*myDeriv
  myDeriv = derivs!i

backProp :: Prog -> V.Vector Value -> V.Vector Derivative
backProp prog@(Prog exprs) values = foldl backPropAtIndex initDerivs (reverse $ indexesOf exprs) where
  backPropAtIndex derivs i = backPropAt prog values i derivs
  initDerivs = vSet (length exprs -1) 1 $ V.replicate (length exprs) 0
