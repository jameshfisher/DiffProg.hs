module Graphviz where

import Data.List (intercalate)
import qualified Data.Vector as V
import Data.Vector ((!))

import Types

exprEdges :: Expr -> [(Index)]
exprEdges e = case e of
  ExprConst d -> []
  ExprAdd indexes -> indexes
  ExprNeg i1 -> [i1]
  ExprMul i1 i2 -> [i1, i2]
  ExprReLU i1 -> [i1]
  ExprLogistic i1 -> [i1]

exprLabel :: Expr -> String
exprLabel e = case e of
  ExprConst d -> show d
  ExprAdd indexes -> "+"
  ExprNeg i1 -> "-"
  ExprMul i1 i2 -> "*"
  ExprReLU i1 -> "relu"
  ExprLogistic i1 -> "log"

progToDot :: Prog -> String
progToDot (Prog exprs) = intercalate "\n" $ ["digraph D {"] ++ nodes ++ edges ++ ["}"] where
  indexes = [0 .. V.length exprs-1]
  nodes :: [String]
  nodes = map (\i -> "N" ++ show i ++ "[label=\"" ++ exprLabel (exprs!i) ++ "\"]") indexes
  edges :: [String]
  edges = concat $ map f $ zip indexes $ V.toList exprs
  f :: (Index, Expr) -> [String]
  f (i,exp) = map g $ exprEdges exp where
    g :: Index -> String
    g edge = "N" ++ show edge ++ "-> N" ++ show i

writeDot :: Prog -> IO ()
writeDot p = writeFile "out.dot" $ progToDot p