module NeuralNet where

import qualified Data.Vector as V
  
import Types

-- A neural net is a kind of prog.
-- It imposes constraints on what kinds of exprs we use,
-- and what order we use them in.

addConstToProg :: Prog -> (Prog, Index)
addConstToProg (Prog exprs) = (Prog $ V.snoc exprs (ExprConst 0), V.length exprs)

addMulToProg :: (Index, Index) -> Prog -> (Prog, Index)
addMulToProg (i1, i2) (Prog exprs) = (Prog $ V.snoc exprs (ExprMul i1 i2), V.length exprs)

addWeightToProg :: (Prog, [Index]) -> Index -> (Prog, [Index])
addWeightToProg (prog0, acc) i = (prog2, acc ++ [mulIndex]) where
  (prog1, constIndex) = addConstToProg prog0
  (prog2, mulIndex) = addMulToProg (i, constIndex) prog1

addWeightsToProg :: [Index] -> Prog -> (Prog, [Index])
addWeightsToProg indexes prog = foldl addWeightToProg (prog, []) indexes

addReLUExprToProg :: Index -> Prog -> (Prog, Index)
addReLUExprToProg index prog@(Prog exprs) = (Prog $ V.snoc exprs $ ExprReLU index, V.length exprs)

addSumExprToProg :: [Index] -> Prog -> (Prog, Index)
addSumExprToProg indexes prog@(Prog exprs) = (Prog $ V.snoc exprs $ ExprAdd indexes, V.length exprs)

-- a neuron is an ensemble of multiplying, adding, and activating (e.g. ReLUing).
addNeuronToProg :: [Index] -> Prog -> (Prog, Index)
addNeuronToProg inputIndexes prog0 = addReLUExprToProg sumIndex prog2 where
  (prog1, weightIndexes) = addWeightsToProg inputIndexes prog0
  (prog2, sumIndex) = addSumExprToProg weightIndexes prog1

addNeuronLayerToProg :: [Index] -> Int -> Prog -> (Prog, [Index])
addNeuronLayerToProg inputIndexes numNeurons prog0 = foldl (addNeuronToProg' inputIndexes) (prog0, []) [1..numNeurons] where
  addNeuronToProg' inputIndexes (prog0, acc) _ = 
    let (prog1, outIndex) = addNeuronToProg inputIndexes prog0
    in (prog1, acc ++ [outIndex])

