{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# options_ghc -Wno-unused-imports #-}
module Main where

import Control.Monad (replicateM)
import Data.Foldable (fold)
import Data.Functor (void)

-- conduit
import qualified Data.Conduit as C (ConduitT, runConduit, yield, await, transPipe)
import Data.Conduit ((.|))
import qualified Data.Conduit.Combinators as C (map, mapM, scanl, scanlM, last, print)
import qualified Data.Conduit.List as C (chunksOf, unfold, unfoldM)
-- containers
import qualified Data.IntMap as IM (IntMap, fromList, insert, lookup, map, mapWithKey, traverseWithKey, foldlWithKey, foldrWithKey)
-- exceptions
import Control.Monad.Catch (MonadThrow(..))
-- -- mnist-idx-conduit
-- import Data.IDX.Conduit (sourceIdxSparse, sBufSize, sNzComponents)
-- splitmix-distributions
import System.Random.SplitMix.Distributions (Gen, GenT, sample, sampleT, bernoulli, normal)
-- transformers
import Control.Monad.Trans.State.Lazy (State, get, put, evalState)
import Control.Monad.Trans.Class (MonadTrans(..))
-- vector
import qualified Data.Vector as V (Vector, toList, fromList, replicate, zip)

import Control.Monad (replicateM)
import Data.RPTree (knn, candidates, Embed(..), Inner(..), RPTree, RPForest, leaves, SVector, fromListSv, DVector, fromListDv, dense, writeCsv, forest, dataSource, sparse, normal2, normalSparse2, datS, datD)
-- import Data.RPTree.Internal.Testing (datS, datD)

main :: IO ()
main = do -- putStrLn "hello!"
  let
    n = 1000
    maxd = 3
    minl = 10
    ntree = 10
    d = 100
    pnz = 0.3
    chunk = 20
    src = datS n d pnz .| C.map (\ x -> Embed x ())
    seed = 1234
  (q, tts) <- sampleT seed $ do
    tts <- C.runConduit $
           forest seed maxd minl ntree chunk pnz d (liftC src)
    q <- sparse 0.3 d (normal 0.1 0.6)
    pure (q, tts)
  let
    res = knn metricL2 1 tts q
  print res


liftC = C.transPipe lift



-- renderTree0 :: Int -> IO ()
renderTree0 tt = do
  let csvrows = V.toList $ fold $ flip evalState A $ traverse labeledV tt -- (tree0 n)
  writeCsv "r/scatter_data.csv" csvrows

-- renderTree1 :: Int -> IO ()
renderTree1 tt = do
  let
    -- csvrows :: [(DVector Double, Pal5)]
    csvrows = fold $ flip evalState A $ traverse labeledV tt -- (tree1 n)
  writeCsv "r/scatter_data_rt2.csv" $ V.toList csvrows

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

-- tree0 :: Int -> RPTree Double (V.Vector (DVector Double))
-- tree0 n = evalGen 1234 $ tree 10 1.0 2 (dataset n)

-- tree1 :: Int -> RT SVector Double (V.Vector (DVector Double))
-- tree1 n = evalGen 1234 $ treeRT 10 20 1.0 2 (dataset n)

dataset :: Int -> V.Vector (DVector Double)
dataset n = V.fromList $ sample 1234 $ replicateM n (dense 2 $ normal 0 1)





srcC :: Monad m => Int -> C.ConduitT i (DVector Double) (GenT m) ()
srcC n = dataSource n normal2


