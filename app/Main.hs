{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# options_ghc -Wno-unused-imports #-}
module Main where

import Control.Monad (replicateM)
import Data.Foldable (fold, toList)
import Data.Functor (void)

-- conduit
import qualified Data.Conduit as C (ConduitT, runConduit, yield, await, transPipe)
import Data.Conduit ((.|))
import qualified Data.Conduit.Combinators as C (map, mapM, scanl, scanlM, last, print, sinkVector, sinkList)
import qualified Data.Conduit.List as C (chunksOf, unfold, unfoldM)
-- containers
import qualified Data.IntMap as IM (IntMap, fromList, insert, lookup, map, mapWithKey, traverseWithKey, foldlWithKey, foldrWithKey)
-- -- exceptions
-- import Control.Monad.Catch (MonadThrow(..))
-- -- mnist-idx-conduit
-- import Data.IDX.Conduit (sourceIdxSparse, sBufSize, sNzComponents)
-- splitmix-distributions
import System.Random.SplitMix.Distributions (Gen, GenT, sample, sampleT, bernoulli, normal)
-- transformers
import Control.Monad.Trans.State.Lazy (State, get, put, evalState)
import Control.Monad.Trans.Class (MonadTrans(..))
-- vector
import qualified Data.Vector as V (Vector, toList, fromList, replicate, zip, zipWith)

import Control.Monad (replicateM)
import Data.RPTree (knn, candidates, rpTreeCfg, RPTreeConfig(..), Embed(..), Inner(..), RPTree, RPForest, SVector, fromListSv, DVector, fromListDv, dense, writeCsv, tree, forest, dataSource, sparse, normal2, normalSparse2, datS, datD, circle2d, treeSize, leafSizes, writeDot)
-- import Data.RPTree.Internal.Testing (datS, datD)

main :: IO ()
main = renderTree0

-- main :: IO ()
-- main = do -- putStrLn "hello!"
--   let
--     n = 1000
--     maxd = 3
--     minl = 10
--     ntree = 10
--     d = 100
--     pnz = 0.3
--     chunk = 20
--     src = datS n d pnz .| C.map (\ x -> Embed x ())
--     -- src = srcCircles n
--     seed = 1234
--   (q, tts) <- sampleT seed $ do
--     tts <- C.runConduit $
--            forest seed maxd minl ntree chunk pnz d (liftC src)
--     q <- sparse 0.3 d (normal 0.1 0.6)
--     pure (q, tts)
--   let
--     res = knn (flip metricL2) 1 tts q
--   print res


-- liftC = C.transPipe lift

embedC :: Monad m => C.ConduitT (v e) (Embed v e ()) m ()
embedC = C.map (\ x -> Embed x ())


-- renderTree0 :: Int -> IO ()
renderTree0 = do
  let
    n = 1000
    dim = 2
    -- maxd = 6
    -- minl = 20
    -- chunk = 50
    (RPCfg maxd minl _ chunk _) = rpTreeCfg n dim
    tt = tree0 n maxd minl chunk
    ttlab = flip evalState A $ traverse labeledV' tt
  print tt -- lab
  --   csvrows = flip evalState A $ traverse labeledV' tt
  -- writeCsv "r/scatter_data_2.csv" csvrows



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

labeledV' :: (Enum b) =>
             V.Vector (Embed v e a)
          -> State b (V.Vector (v e, b))
labeledV' xs = do
  i <- get
  put (succ i)
  let
    n = length xs
    f (Embed x _) ii = (x, ii)
  pure $ V.zipWith f xs (V.replicate n i)

data Pal5 = A | B | C | D | E deriving (Eq, Show)
instance Enum Pal5 where
  toEnum = \case
    0 -> A
    1 -> B
    2 -> C
    3 -> D
    4 -> E
    x -> toEnum (x `mod` 6)
  fromEnum = \case
    A -> 0
    B -> 1
    C -> 2
    D -> 3
    E -> 4

tree0dot :: IO ()
tree0dot = writeDot f fpath "tree0" $ tree0 10000 6 10 50
  where
    f = show . length
    fpath = "tree0.dot"

tree0 :: Int -- ^ dataset size
      -> Int -- ^ max tree depth
      -> Int -- ^ min leaf size
      -> Int -- ^ chunk size
      -> RPTree Double () (V.Vector (Embed DVector Double ()))
tree0 n maxd minl chunk = sample s $ tree s maxd minl chunk 1.0 2 (srcC n .| embedC)
  where
    s = 123513

dataset :: Int -> V.Vector (DVector Double)
dataset n = V.fromList $ sample 1234 $ replicateM n (dense 2 $ normal 0 1)

datasetCircles :: Int -> V.Vector (DVector Double)
datasetCircles n = V.fromList $ sample 1234 $ C.runConduit $ srcCircles n .| C.sinkList



srcC :: Monad m => Int -> C.ConduitT i (DVector Double) (GenT m) ()
srcC n = dataSource n normal2

srcCircles :: Monad m =>
              Int -> C.ConduitT i (DVector Double) (GenT m) ()
srcCircles n = dataSource n circle2d2

-- binary mixture of two non-overlapping circles
circle2d2 :: (Monad m) => GenT m (DVector Double)
circle2d2 = do
  let
    d = fromListDv [2, 3]
    r = 1
  b <- bernoulli 0.5
  if b
    then circle2d r
    else (^+^ d) <$> circle2d r


-- -- renderTree1 :: Int -> IO ()
-- renderTree1 tt = do
--   let
--     -- csvrows :: [(DVector Double, Pal5)]
--     csvrows = fold $ flip evalState A $ traverse labeledV tt -- (tree1 n)
--   writeCsv "r/scatter_data_rt2.csv" $ V.toList csvrows
