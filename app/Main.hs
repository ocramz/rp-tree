{-# LANGUAGE LambdaCase #-}
{-# options_ghc -Wno-unused-imports #-}
module Main where

import Data.Foldable (fold)
-- transformers
import Control.Monad.Trans.State.Lazy (State, get, put, evalState)
-- vector
import qualified Data.Vector as V (Vector, toList, fromList, replicate, zip)

import Control.Monad (replicateM)
import Data.RPTree (Inner(..), RPTree, tree, tree', RT, treeRT2, leaves, SVector, DVector, Gen, GenT, dense, bernoulli, normal, evalGen, evalGenT, writeCsv)

main :: IO ()
main = do
  let
    n = 10000
  renderTree0 n
  renderTree1 n


renderTree0 :: Int -> IO ()
renderTree0 n = do
  let csvrows = V.toList $ fold $ flip evalState A $ traverse labeledV (tree0 n)
  writeCsv "r/scatter_data.csv" csvrows

renderTree1 :: Int -> IO ()
renderTree1 n = do
  let
    csvrows :: [(DVector Double, Pal5)]
    csvrows = fold $ flip evalState A $ traverse labeled (tree1 n)
  writeCsv "r/scatter_data_rt2.csv" csvrows  


labeled :: (Enum i) =>
           [a] -> State i [(a, i)]
labeled xs = do
  i <- get
  put (succ i)
  let n = length xs
  pure $ zip xs (replicate n i)


labeledV :: Enum i => V.Vector a -> State i (V.Vector (a, i))
labeledV xs = do
  i <- get
  put (succ i)
  let n = length xs
  pure $ V.zip xs (V.replicate n i)

data Pal5 = A | B | C | D | E deriving (Eq, Show)
instance Enum Pal5 where
  toEnum = \case
    0 -> A
    1 -> B
    2 -> C
    3 -> D
    4 -> E
    x -> toEnum (x `mod` 5)
  fromEnum = \case
    A -> 0
    B -> 1
    C -> 2
    D -> 3
    E -> 4

tree0 :: Int -> RPTree Double (V.Vector (DVector Double))
tree0 n = evalGen 1234 $ tree 10 1.0 2 (dataset n)

tree1 :: Int -> RT DVector Double [DVector Double]
tree1 n = evalGen 1234 $ treeRT2 10 2 (dataset n)

dataset :: Int -> [DVector Double]
dataset n = evalGen 1234 $ replicateM n (dense 2 $ normal 0 1)

normal2 :: (Monad m) => GenT m (DVector Double)
normal2 = do
  b <- bernoulli 0.5
  if b
    then dense 2 $ normal 0 0.5
    else dense 2 $ normal 2 0.5
