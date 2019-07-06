module Test where

import qualified Data.Vector as V
import Test.QuickCheck
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, oneof, choose, listOf)
import Control.Monad (replicateM)
  
import Types
import Eval
import Helpers
import BackProp

arbitraryExprConst = ExprConst <$> (choose (-10, 10) :: Gen Double)

arbitraryExpr :: Index -> Gen Expr
arbitraryExpr maxIndex = oneof [
    arbitraryExprConst,
    do { len <- choose (0, 10); l <- replicateM len arbitraryIndex; return $ ExprAdd l },
    ExprNeg <$> arbitraryIndex,
    ExprMul <$> arbitraryIndex <*> arbitraryIndex,
    ExprReLU <$> arbitraryIndex,
    ExprLogistic <$> arbitraryIndex
  ] where
  arbitraryIndex = choose (0,maxIndex)

arbitraryRecursiveProg = do
  Prog subProg <- arbitrary
  expr <- arbitraryExpr $ length subProg - 1
  return $ Prog $ V.snoc subProg expr

instance Arbitrary Prog where
  arbitrary = oneof [ 
    (\c -> Prog $ V.fromList [c]) <$> arbitraryExprConst, 
    arbitraryRecursiveProg 
    ]

evalProgWithDelta :: Index -> Value -> Prog -> V.Vector Value
evalProgWithDelta i delta prog@(Prog exprs) = V.foldl' f V.empty exprs where
  f vals expr = V.snoc vals $ evalExpr vals expr + (if i == length vals then delta else 0)

poorManDerivAt :: Prog -> Index -> Derivative
poorManDerivAt prog@(Prog exprs) i = (after-before) / delta where
  before = V.last $ evalProg prog
  after = V.last $ evalProgWithDelta i delta prog
  delta = 0.00000001

poorManBackProp :: Prog -> V.Vector Value -> V.Vector Derivative
poorManBackProp prog@(Prog exprs) _ = V.fromList $ map (\i -> poorManDerivAt prog i) (indexesOf exprs)

-- Yes, it fails occasionally, because
-- a) the MODEL is not completely accurate; poor man's back prop is quite inaccurate for some progs
-- b) ReLU is not differentiable at 0
propModelCheckBackProp :: Prog -> Bool
propModelCheckBackProp prog@(Prog exprs) = sumError < 3 where
  sumError = V.sum errors
  errors = V.zipWith (\x y -> abs (x-y)) poorResult richResult
  poorResult = poorManBackProp prog values
  richResult = backProp prog values
  values = evalProg prog

main :: IO ()
main = quickCheck (withMaxSuccess 10000 propModelCheckBackProp)