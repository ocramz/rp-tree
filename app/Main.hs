{-# options_ghc -Wno-unused-imports #-}
module Main where

import Data.Foldable (fold)
-- transformers
import Control.Monad.Trans.State.Lazy (State, get, put, evalState)

import Control.Monad (replicateM)
import Data.RPTree (Inner(..), RPTree, tree, leaves, SVector, DVector, Gen, GenT, dense, bernoulli, normal, evalGen, evalGenT, writeCsv)

main :: IO ()
main = do -- putStrLn "hello!"
  let
    n = 10000
    csvrows :: [(DVector Double, Char)]
    csvrows = fold $ flip evalState 'a' $ traverse labeled (tree0 n)
  writeCsv "r/scatter_data.csv" csvrows


labeled :: Enum i => [a] -> State i [(a, i)]
labeled xs = do
  i <- get
  put (succ i)
  let n = length xs
  pure $ zip xs (replicate n i)


tree0 :: Int -> RPTree Double [DVector Double]
tree0 n = evalGen 1234 $ tree 10 1.0 2 (dataset n)

dataset :: Int -> [DVector Double]
dataset n = evalGen 1234 $ replicateM n (dense 2 $ normal 0 1)

normal2 :: (Monad m) => GenT m (DVector Double)
normal2 = do
  b <- bernoulli 0.5
  if b
    then dense 2 $ normal 0 0.5
    else dense 2 $ normal 2 0.5
