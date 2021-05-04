{-# LANGUAGE LambdaCase #-}
{-# options_ghc -Wno-unused-imports #-}
module Main where

import Data.Foldable (fold)
-- transformers
import Control.Monad.Trans.State.Lazy (State, get, put, evalState)
-- vector
import qualified Data.Vector as V (Vector, toList, fromList, replicate, zip)

import Control.Monad (replicateM)
import Data.RPTree (Inner(..), RPTree, tree, tree', leaves, SVector, DVector, Gen, GenT, dense, bernoulli, normal, evalGen, evalGenT, writeCsv)

main :: IO ()
main = do -- putStrLn "hello!"
  let
    n = 10000
    csvrows :: [(DVector Double, Pal3)]
    csvrows = V.toList $ fold $ flip evalState A $ traverse labeled (tree0 n)
  writeCsv "r/scatter_data.csv" csvrows


labeled :: Enum i => V.Vector a -> State i (V.Vector (a, i))
labeled xs = do
  i <- get
  put (succ i)
  let n = length xs
  pure $ V.zip xs (V.replicate n i)

data Pal3 = A | B | C | D | E deriving (Eq, Show)
instance Enum Pal3 where
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
tree0 n = evalGen 1234 $ tree' 5 1.0 2 (dataset n)

dataset :: Int -> [DVector Double]
dataset n = evalGen 1234 $ replicateM n (dense 2 $ normal 0 1)

normal2 :: (Monad m) => GenT m (DVector Double)
normal2 = do
  b <- bernoulli 0.5
  if b
    then dense 2 $ normal 0 0.5
    else dense 2 $ normal 2 0.5
