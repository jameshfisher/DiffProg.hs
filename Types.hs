module Types where

import qualified Data.Vector as V

type Index = Int
type Value = Double
type Derivative = Double

data Expr
  = ExprConst Value
  | ExprAdd [Index]
  | ExprNeg Index
  | ExprMul Index Index
  | ExprReLU Index
  | ExprLogistic Index
  
  deriving (Show)

newtype Prog = Prog (V.Vector Expr)
  deriving (Show)